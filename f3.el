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
  (let ((texts (helm-mm-split-pattern pat)))
    (cl-reduce
     (lambda (parsed1 parsed2) (list :and parsed1 parsed2))
     (cl-mapcar (lambda (pat) (list :regex pat)) texts))))

(defun f3-create-find-pattern (pat)
  "Assumes correctness of pattern PAT."
  (list :raw pat))

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
    (`(:raw ,thing) (helm-mm-split-pattern thing))
    (`(:filetype ,thing) (list "-type" thing))
    (_ parsed-args)))

(defcustom f3-default-combinator :and
  "Default combinator for multiple `f3' patterns."
  :group 'f3)
(defvar f3-current-combinator f3-default-combinator)
(defcustom f3-default-mode :text
  "Default input mode for `f3' patterns."
  :group 'f3)
(defvar f3-current-mode f3-default-mode)

;;; TODO: restart `helm-pattern' with whatever was in it before `f3-open-paren'
;;; or `f3-close-paren' was called, after calling it
(defun f3-open-paren (dir cur-cmd paren-stack)
  (f3-do dir nil (cons (list f3-current-combinator cur-cmd) paren-stack)
         f3-current-combinator f3-current-mode))

(defun f3-close-paren (dir cur-cmd paren-stack)
  (let ((cur-paren (car paren-stack))
        (upper (cdr paren-stack)))
    (cl-destructuring-bind (comb cmd) cur-paren
      (f3-do dir (list comb cmd cur-cmd) upper f3-current-combinator
             f3-current-mode))))

(defun f3-close-parens-to-make-command (cur-cmd paren-stack)
  (cl-reduce
   (lambda (cur stack-el)
     (cl-destructuring-bind (comb cmd) stack-el (list comb cur cmd)))
   paren-stack
   :initial-value cur-cmd))

(defvar f3-current-complement nil)

(defcustom f3-find-program "find"
  "Default command to find files with using `f3'."
  :group 'f3)

(defvar f3-find-directory nil)
(defvar f3-current-command nil)

(defconst f3-proc-name "*f3-find*")
(defconst f3-buf-name "*f3-find-output*")
(defconst f3-err-buf-name "*f3-errors*")

(defun f3-make-process ()
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
            current-pattern))
         (complete-pattern
          (f3-close-parens-to-make-command combined-pattern nil)))
    (when complete-pattern
      (let* ((find-dir
              (file-relative-name f3-find-directory))
             (args-list (cons find-dir (f3-parsed-to-command complete-pattern))))
        (progn
          (message "args: %S" args-list)
          (make-process
           :name f3-proc-name
           :buffer f3-buf-name
           :command (cons f3-find-program args-list)
           :stderr f3-err-buf-name))))))

(defconst f3-async-candidate-limit 2000)

;;; TODO: make this work!
;; (defvar f3-buffer-source
;;   (helm-build-in-buffer-source)
;;   "Source searching currently open buffer names for results.")

(defvar f3-find-process-source
  (helm-build-async-source "f3 find"
    :candidates-process #'f3-make-process
    :candidate-number-limit f3-async-candidate-limit))

;;; TODO: make defcustom for f3's default directory: either the current
;;; directory of the current buffer, the "project directory," or some other
;;; given directory, or a function that returns a directory path given the
;;; current file name
;;; TODO: add function to search through open buffers as well; this use case
;;; should be optimized for

(defconst f3-helm-buffer-name "*f3*")

;;;###autoload
(defun f3 (start-dir)
  ;; TODO: get the right directory, not just `default-directory'
  (interactive (list default-directory))
  ;; in case user modifies defcustoms after this file is loaded
  (setq
   f3-current-combinator f3-default-combinator
   f3-current-mode f3-default-mode
   f3-current-complement nil            ; no complement at start
   f3-find-directory start-dir
   f3-current-command nil)
  (helm :sources '(f3-find-process-source) :buffer f3-helm-buffer-name))

(provide 'f3)
;;; f3.el ends here
