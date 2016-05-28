;;; f3.el --- helm interface for searching files really fast -*- lexical-binding: t -*-

;; Keywords: find, files, helm

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'helm)

(defgroup f3 nil
  "Group for `f3' customizations.")

;;; functions to parse patterns into an AST
;;; TODO: persist input mode across combinators
(defconst f3-input-modes
  '((:text . f3-create-text-pattern)
    (:regex . f3-create-regex-pattern)
    (:find . f3-create-find-pattern)
    (:filetype . f3-create-filetype-pattern))
  "Modes which interpret the current `helm-pattern' differently.")

(defun f3-create-text-pattern (pat)
  (let ((texts (helm-mm-split-pattern pat)))
    (cl-reduce
     (lambda (parsed1 parsed2) (list :and parsed1 parsed2))
     (cl-mapcar
      (lambda (pat)
        (list :text (format "*%s*" pat)))
      texts))))

(defun f3-create-regex-pattern (pat)
  (let ((texts (helm-mm-3-get-patterns pat)))
    (cl-reduce
     (lambda (parsed1 parsed2) (list :and parsed1 parsed2))
     (cl-mapcar
      (lambda (pat)
        (cl-destructuring-bind (pred . reg) pat
          (let ((real-reg (format "\\(\\`\\|.*\\)%s\\(.*\\|\\'\\)" reg)))
            (if (eq pred 'identity)
                `(:regex ,real-reg)
              `(:not (:regex ,real-reg))))))
      texts))))

(defun f3-create-find-pattern (pat)
  "Assumes correctness of pattern PAT."
  (let ((split-pattern
         (cl-mapcar
          (lambda (str)
            (replace-regexp-in-string "\\`['\"]\\|['\"]\\'" "" str))
          (helm-mm-split-pattern pat))))
    (list :raw split-pattern)))

(defconst f3-valid-filetype-patterns '("b" "c" "d" "f" "l" "p" "s")
  "Valid filetype arguments to unix find.")

(defun f3-create-filetype-pattern (pat)
  (unless (member pat f3-valid-filetype-patterns)
    (user-error (format "%s '%s'" "invalid filetype pattern" pat)))
  (list :filetype pat))

(defconst f3-combinators '(:and :or))

(defun f3-do-complement (ast do-complement)
  (if do-complement (list :not ast) ast))

(defun f3-pattern-to-parsed-arg (pattern mode complement)
  (let ((process-input-fn (cdr (assoc mode f3-input-modes))))
    (if process-input-fn
        (f3-do-complement (funcall process-input-fn pattern) complement)
      (user-error (format "%s '%S'" "invalid mode" mode)))))

;;; TODO: do i?wholename and friends as well
(defun f3-maybe-lowercase-generate (base pat)
  (if (string-match-p "[[:upper:]]" pat)
      (list (format "-%s" base) pat)
    (list (format "-i%s" base) pat)))

(defun f3-parsed-to-command (parsed-args)
  "Transform PARSED-ARGS to a raw find command."
  (pcase parsed-args
    ;; operators
    (`(:and . ,args)
     (cl-reduce (lambda (arg1 arg2)
                  (append (list "(") arg1 (list ")" "-and")
                          (list "(") arg2 (list ")")))
                (cl-mapcar #'f3-parsed-to-command args)))
    (`(:or . ,args)
     (cl-reduce (lambda (arg1 arg2)
                  (append (list "(") arg1 (list ")" "-or")
                          (list "(") arg2 (list ")")))
                (cl-mapcar #'f3-parsed-to-command args)))
    (`(:not ,thing)
     (append (list "-not" "(") (f3-parsed-to-command thing) (list ")")))
    ;; modes
    (`(:text ,thing) (f3-maybe-lowercase-generate "name" thing))
    (`(:regex ,thing) (f3-maybe-lowercase-generate "regex" thing))
    (`(:raw ,thing) (append (list "(") thing (list ")")))
    (`(:filetype ,thing) (list "-type" thing))
    (_ parsed-args)))

(defcustom f3-default-combinator :and
  "Default combinator for multiple `f3' patterns."
  :group 'f3)
(defvar f3-current-combinator f3-default-combinator)
(defcustom f3-default-mode :regex
  "Default input mode for `f3' patterns."
  :group 'f3)
(defvar f3-current-mode f3-default-mode)

;;; TODO: restart `helm-pattern' with whatever was in it before `f3-open-paren'
;;; or `f3-close-paren' was called, after calling it
(defvar f3-current-complement nil)

(defcustom f3-find-program "find"
  "Default command to find files with using `f3'."
  :group 'f3)

(defvar f3-find-directory nil)
(defvar f3-current-command nil)

(defconst f3-proc-name "*f3-find*")
(defconst f3-buf-name "*f3-find-output*")
(defconst f3-err-buf-name "*f3-errors*")

(defconst f3-candidate-limit 2000)

;;; matches buffers strictly by `helm-pattern', and only when no combinators are
;;; used
;;; TODO: switch this off whenever a combinator is turned on
(defvar f3-match-buffers t
  "Whether to match buffers as well as async find results.")

(defun f3-get-buffer-names ()
  (when f3-match-buffers
    (mapcar
     (lambda (buf) (cons (buffer-name buf) buf))
     (cl-remove-if-not #'buffer-file-name (buffer-list)))))

(defun f3-buffer-persistent-action (buf)
  (switch-to-buffer buf)
  (let ((start (line-beginning-position))
        (end (1+ (line-end-position))))
    (if (not helm-match-line-overlay)
        (setq helm-match-line-overlay (make-overlay start end buf))
      (move-overlay helm-match-line-overlay start end buf))
    (overlay-put helm-match-line-overlay 'face 'helm-selection-line)))

(defun f3-get-ast ()
  (let ((res
         (let* ((current-pattern
                 (unless (string-empty-p helm-pattern)
                   (f3-pattern-to-parsed-arg
                    helm-pattern f3-current-mode f3-current-complement)))
                (combined-pattern
                 (if f3-current-command
                     (if current-pattern
                         (list f3-current-combinator f3-current-command
                               current-pattern)
                       f3-current-command)
                   current-pattern)))
           (f3-close-parens-to-make-command combined-pattern nil))))
    ;; (message "ast: %S" res)
    res))

(defun f3-filter-buffer-candidates (cand)
  (cl-case f3-current-mode
    (:text (helm-mm-3-match
            cand
            (replace-regexp-in-string
             "\\(\s-*\\)!" "\\1\\\\!" (regexp-quote helm-pattern))))
    (:regex (helm-mm-3-match cand))
    (t nil)))

(defvar f3-buffer-source
  (helm-build-sync-source "f3 buffer search"
    :candidates #'f3-get-buffer-names
    :match #'f3-filter-buffer-candidates
    :candidate-number-limit f3-candidate-limit
    :action (helm-make-actions "Visit" #'switch-to-buffer)
    :persistent-action #'f3-buffer-persistent-action)
  "Source searching currently open buffer names for results.")

(defun f3-make-process ()
  (let ((final-pat (f3-get-ast)))
    (when final-pat
      (let* ((find-dir
              (file-relative-name f3-find-directory))
             (args-list (cons find-dir (f3-parsed-to-command final-pat))))
        (message "args: %S" args-list)
        (make-process
         :name f3-proc-name
         :buffer f3-buf-name
         :command (cons f3-find-program args-list)
         :stderr f3-err-buf-name)))))

(defvar f3-find-process-source
  (helm-build-async-source "f3 find"
    :candidates-process #'f3-make-process
    :candidate-number-limit f3-candidate-limit))

;;; TODO: make defcustom for f3's default directory: either the current
;;; directory of the current buffer, the "project directory," or some other
;;; given directory, or a function that returns a directory path given the
;;; current file name

(defconst f3-helm-buffer-name "*f3*")

(defun f3-do (start-dir prev-cmd)
  (setq f3-find-directory start-dir
        f3-current-command prev-cmd
        f3-buffer-matcher nil)
  ;; FIXME remove all red matches from `helm-highlight-current-line'; all on
  ;; current line still remain
  (let ((helm-highlight-matches-around-point-max-lines nil))
    (helm :sources '(f3-find-process-source f3-buffer-source)
          :buffer f3-helm-buffer-name)))

;;;###autoload
(defun f3 (start-dir)
  ;; TODO: get the right directory, not just `default-directory'
  (interactive (list default-directory))
  (setq
   f3-current-combinator f3-default-combinator
   f3-current-mode f3-default-mode
   f3-current-complement nil)
  (f3-do start-dir nil))

(provide 'f3)
;;; f3.el ends here
