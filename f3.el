;;; f3.el --- The Fantastic File Finder: a helm interface for searching files really fast -*- lexical-binding: t -*-

;; Keywords: find, files, helm

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'helm)

(defgroup f3 nil
  "Group for `f3' customizations.")

;;; TODO: ignore commonly ignored files/folders (.git, anything in .gitignore,
;;; etc)

;;; functions to parse patterns into an AST
;;; TODO: persist input mode across combinators
(defconst f3-input-modes
  '((:text . f3-create-text-pattern)
    (:regex . f3-create-regex-pattern)
    (:raw . f3-create-raw-pattern)
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

(defun f3-create-raw-pattern (pat)
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
    (error (format "%s '%s'" "invalid filetype pattern" pat)))
  (list :filetype pat))

(defconst f3-combinators '(:and :or))

(defun f3-do-complement (ast do-complement)
  (if do-complement (list :not ast) ast))

(defun f3-pattern-to-parsed-arg (pattern mode complement)
  (let ((process-input-fn (cdr (assoc mode f3-input-modes))))
    (if process-input-fn
        (f3-do-complement (funcall process-input-fn pattern) complement)
      (error (format "%s '%S'" "invalid mode" mode)))))

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
    (`(:paren ,thing) (append (list "(") thing (list ")")))
    ;; modes
    (`(:text ,thing) (f3-maybe-lowercase-generate "name" thing))
    (`(:regex ,thing) (f3-maybe-lowercase-generate "regex" thing))
    (`(:raw ,thing) (append (list "(") thing (list ")")))
    (`(:filetype ,thing) (list "-type" thing))
    (_ (error (format "cannot comprehend arguments %S" parsed-args)))))

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
                    helm-pattern f3-current-mode f3-current-complement))))
           (or (and f3-current-command current-pattern
                    (list f3-current-combinator f3-current-command
                          current-pattern))
               f3-current-command
               current-pattern))))
    ;; (message "ast: %S" res)
    res))

(defun f3-filter-buffer-candidates (cand)
  (cl-case f3-current-mode
    (:text (helm-mm-3-match
            cand
            (replace-regexp-in-string
             "\\(\\s-*\\)!" "\\1\\\\!" (regexp-quote helm-pattern))))
    (:regex (helm-mm-3-match cand))
    (t nil)))

(defvar f3-buffer-source
  (helm-build-sync-source "open buffers"
    :candidates #'f3-get-buffer-names
    :match #'f3-filter-buffer-candidates
    :candidate-number-limit f3-candidate-limit
    :action (helm-make-actions "Visit" #'switch-to-buffer)
    :persistent-action #'f3-buffer-persistent-action)
  "Source searching currently open buffer names for results.")

(defun f3-make-process ()
  (let ((final-pat (f3-get-ast)))
    (when final-pat
      (with-current-buffer f3-source-buffer
        (let ((args (append (list f3-find-program ".")
                            (f3-parsed-to-command final-pat)))
              (default-directory f3-cached-dir))
          (message "default-directory: %s, args: %S" default-directory args)
          (make-process
           :name f3-proc-name
           :buffer f3-buf-name
           :command args
           :stderr f3-err-buf-name))))))

(defvar f3-currently-opened-persistent-buffers nil)

(defun f3-clear-opened-persistent-buffers ()
  (cl-mapc #'kill-buffer f3-currently-opened-persistent-buffers)
  (setq f3-currently-opened-persistent-buffers nil))

(defun f3-async-filter-function (cand)
  "Remove leading './' from candidate CAND."
  (substring cand 2))

(defun f3-async-display-to-real (cand)
  (with-current-buffer f3-source-buffer
    (let* ((default-directory f3-cached-dir)
           (buf (find-buffer-visiting cand)))
      (unless buf
        (setq buf (find-file cand))
        (push buf f3-currently-opened-persistent-buffers))
      buf)))

(defvar f3-find-process-source
  (helm-build-async-source "find"
    :candidates-process #'f3-make-process
    :candidate-number-limit f3-candidate-limit
    :action (helm-make-actions "Visit" #'switch-to-buffer)
    :persistent-action #'f3-buffer-persistent-action
    :filter-one-by-one #'f3-async-filter-function
    :display-to-real #'f3-async-display-to-real
    :cleanup #'f3-clear-opened-persistent-buffers)
  "Source searching files within a given directory using the find command.")

;;; TODO: make defcustom for f3's default directory: either the current
;;; directory of the current buffer, the "project directory," or some other
;;; given directory, or a function that returns a directory path given the
;;; current file name

(defconst f3-helm-buffer-name "*f3*")

(defvar f3-last-selected-candidate nil)

(defvar-local f3-cached-dir nil)

;;; TODO: implement this!!! use some external library
(defun f3-use-project-dir (from)
  (error "can't find project dir!"))

(defun f3-use-file-dir (_)
  (with-current-buffer (or f3-source-buffer (current-buffer))
    default-directory))

(defun f3-explicitly-choose-dir (from)
  (expand-file-name (read-directory-name "Directory to search: " from nil t)))

(defun f3-up-dir (from)
  (expand-file-name (format "%s/../" from)))

(defcustom f3-default-directory 'project
  "Default directory to set as pwd when running `f3'. 'project for the project
directory, 'choose to choose every time, and nil (or anything else) to choose
the current directory of the buffer in which `f3' is run. See the source of
`f3-choose-dir' for details."
  :group 'f3
  :type 'symbol)

(defvar f3-source-buffer nil)

(defun f3-choose-dir ()
  (with-current-buffer (or f3-source-buffer (current-buffer))
    (or f3-cached-dir
        (setq f3-cached-dir
              (cl-case f3-default-directory
                (project (f3-use-project-dir))
                (choose (f3-explicitly-choose-dir))
                (t default-directory))))))

(defun f3-choose-dir-and-rerun (dir-fun)
  (lambda ()
    (interactive)
    (with-current-buffer f3-source-buffer
      (let ((cmd f3-current-command)
            (pat helm-pattern))
        (helm-run-after-exit
         (lambda ()
           (setq f3-cached-dir (funcall dir-fun f3-cached-dir))
           (f3-do f3-cached-dir cmd pat)))))))

(defconst f3-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-p") (f3-choose-dir-and-rerun #'f3-use-project-dir))
    (define-key map (kbd "M-i") (f3-choose-dir-and-rerun #'f3-use-file-dir))
    (define-key map (kbd "M-f")
      (f3-choose-dir-and-rerun #'f3-explicitly-choose-dir))
    (define-key map (kbd "M-j") (f3-choose-dir-and-rerun #'f3-up-dir))
    map)
  "Keymap for `f3'.")

(defun f3-do (start-dir prev-cmd &optional initial-input)
  ;; FIXME: remove all red matches from `helm-highlight-current-line'; all on
  ;; current line still remain
  (let ((f3-current-command prev-cmd)
        (f3-buffer-matcher nil)
        (helm-highlight-matches-around-point-max-lines nil)
        (last-cand
         (and (stringp f3-last-selected-candidate)
              (buffer-live-p (get-buffer f3-last-selected-candidate))
              f3-last-selected-candidate))
        (prompt (format "%s: " (substring (symbol-name f3-current-mode) 1))))
    (helm-attrset 'name (format "find @ %s" start-dir) f3-find-process-source)
    (setq f3-last-selected-candidate
          (helm :sources '(f3-find-process-source f3-buffer-source)
                :buffer f3-helm-buffer-name
                :input (or initial-input "")
                :preselect last-cand
                :prompt prompt
                :keymap f3-map
                :default-directory start-dir))))

;;;###autoload
(defun f3 (start-dir)
  (interactive (list (f3-choose-dir)))
  (let ((f3-source-buffer (current-buffer))
        (f3-current-combinator f3-default-combinator)
        (f3-current-mode f3-default-mode)
        (f3-current-complement nil))
    (f3-do start-dir nil)))

(provide 'f3)
;;; f3.el ends here
