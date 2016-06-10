;;; f3.el --- The Fantastic File Finder: a helm interface for searching files really fast -*- lexical-binding: t -*-

;; Author: Danny McClanahan <danieldmcclanahan@gmail.com>
;; Version 0.1
;; Package-Requires: ((emacs "24") (helm "1.9.6") (cl-lib "0.5"))
;; Keywords: find, file, files, helm, fast, finder


;;; Commentary:

;; Focuses on two use cases:
;; 1. Finding a file in a project really fast.
;; 2. Finding some complex set of files and performing some action on them.


;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'helm-multi-match)
(require 'subr-x)


;; Customization
(defgroup f3 nil "Group for `f3' customizations.")

(defcustom f3-default-mode :text
  "Default input mode for `f3' patterns."
  :type 'symbol
  :group 'f3)

(defcustom f3-find-program "find"
  "Default command to find files with using `f3'."
  :group 'f3)

(defcustom f3-default-directory 'project
  "Default directory to set as pwd when running `f3'. 'project for the project
directory, 'choose to choose every time, and nil (or anything else) to choose
the current directory of the buffer in which `f3' is run. See the source of
`f3-choose-dir' for details. Can be a function accepting the current buffer and
returning a directory path."
  :type '(choice symbol function)
  :group 'f3)


;; Constants
(defconst f3-input-modes
  '((:text . f3-create-text-pattern)
    (:regex . f3-create-regex-pattern)
    (:raw . f3-create-raw-pattern)
    (:filetype . f3-create-filetype-pattern)
    (:perm . f3-create-perm-pattern))
  "Modes which interpret the current `helm-pattern' differently.")

(defconst f3-valid-filetype-patterns '("b" "c" "d" "f" "l" "p" "s")
  "Valid filetype arguments to unix find.")

(defconst f3-combinators '(:and :or))

(defconst f3-helm-buffer-name "*f3*")
(defconst f3-proc-name "*f3-find*")
(defconst f3-buf-name "*f3-find-output*")

(defconst f3-candidate-limit 2000)

(defconst f3-err-proc-name "*f3-err-proc*")
(defconst f3-err-buf-name "*f3-errors*")
(defconst f3-err-msg-props '(font-lock-warning-face :height 2.0))

(defconst f3-start-anchors '("\\`" "^"))
(defconst f3-end-anchors '("\\'" "$"))
(defconst f3-shell-wildcards '("*"))


;; Global variables
(defvar f3-current-mode)

(defvar f3-current-complement)

(defvar f3-current-command nil)

(defvar f3-match-buffers t
  "Whether to match buffers as well as async find results. Starts on, turned off
within a session after a combinator is used.")

(defvar f3-buffer-source
  (helm-build-sync-source "open buffers"
    :candidates #'f3-get-buffer-names
    :match-strict #'f3-filter-buffer-candidates
    :candidate-number-limit f3-candidate-limit
    :action (helm-make-actions "Visit" #'f3-sync-action)
    :persistent-action #'f3-buffer-persistent-action)
  "Source searching currently open buffer names for results.")

(defvar f3-currently-opened-persistent-buffers nil
  "The buffers opened through a persistent action within an `f3' session. These
are killed at the end of a session.")

(defvar f3-find-process-source
  (helm-build-async-source "find"
    :candidates-process #'f3-make-process
    :candidate-number-limit f3-candidate-limit
    :action (helm-make-actions "Visit" #'f3-async-action)
    :persistent-action #'f3-async-persistent-action
    :filter-one-by-one #'f3-async-filter-function
    :cleanup #'f3-clear-opened-persistent-buffers)
  "Source searching files within a given directory using the find command.")

(defvar f3-last-selected-candidate nil
  "Buffer which was last selected through `f3'.")

(defvar f3-source-buffer nil
  "Buffer which is current when `f3' is invoked.")

(defvar f3-current-operator-stack nil
  "Current stream of parsed operators.")

(defvar f3-current-redo-stack nil
  "Current stream of parsed operators along with any hanging after undos.")


;; Buffer-local variables
(defvar-local f3-cached-dir nil)


;; Functions
(defun f3-wildcard-unless-meta (pat start-anchors end-anchors insert)
  "Surround PAT with INSERT on both sides, unless PAT has an anchor on either
side (as denoted by lists START-ANCHORS and END-ANCHORS)."
  (let ((start-pat
         (if (cl-some
              (lambda (anch)
                (let ((case-fold-search nil))
                  (string-match-p (concat "\\`" (regexp-quote anch)) pat)))
              start-anchors)
             pat
           (concat insert pat))))
    (if (cl-some
         (lambda (anch)
           (let ((case-fold-search nil))
             (string-match-p (concat (regexp-quote anch) "\\'") start-pat)))
         end-anchors)
        start-pat
      (concat start-pat insert))))

(defun f3-shell-expansion-unless-wildcard (pat)
  (f3-wildcard-unless-meta pat f3-shell-wildcards f3-shell-wildcards "*"))

(defun f3-create-text-pattern (pat)
  (let ((texts (helm-mm-split-pattern pat)))
    (cl-reduce
     (lambda (parsed1 parsed2) `(:and ,parsed1 ,parsed2))
     (cl-mapcar
      (lambda (pat)
        `(:text ,(f3-shell-expansion-unless-wildcard pat)))
      texts))))

(defun f3-dot-star-unless-anchor (pat)
  (f3-wildcard-unless-meta pat f3-start-anchors f3-end-anchors ".*"))

(defun f3-create-regex-pattern (pat)
  (let ((texts (helm-mm-3-get-patterns pat)))
    (cl-reduce
     (lambda (parsed1 parsed2) `(:and ,parsed1 ,parsed2))
     (cl-mapcar
      (lambda (pat)
        (cl-destructuring-bind (pred . reg) pat
          (let ((real-reg (f3-dot-star-unless-anchor reg)))
            (if (eq pred 'identity)
                `(:regex ,real-reg)
              `(:not (:regex ,real-reg))))))
      texts))))

(defun f3-create-raw-pattern (pat)
  "Assumes correctness of pattern PAT."
  (let ((split-pattern
         (with-temp-buffer
           (push-mark)
           (insert pat)
           (car (shell--parse-pcomplete-arguments)))))
    `(:raw ,split-pattern)))

(defun f3-create-filetype-pattern (pat)
  `(:filetype ,pat))

(defun f3-create-perm-pattern (pat)
  `(:perm ,pat))

(defun f3-do-complement (ast do-complement)
  (if do-complement `(:not ,ast) ast))

(defun f3-pattern-to-parsed-arg (pattern)
  (let ((process-input-fn (cdr (assoc f3-current-mode f3-input-modes))))
    (if process-input-fn
        (f3-do-complement (funcall process-input-fn pattern)
                          f3-current-complement)
      (error (format "%s '%S'" "invalid mode" f3-current-mode)))))

(defun f3-maybe-lowercase-generate (base pat)
  (let ((case-fold-search nil))
    (if (string-match-p "[[:upper:]]" pat)
        (list (format "-%s" base) pat)
      (list (format "-i%s" base) pat))))

;;; TODO: fix helm's automatic highlighting of results; maybe use some logic in
;;; `f3-filter-buffer-candidates'?
(defun f3-parsed-to-command (parsed-args)
  "Transform PARSED-ARGS to a raw find command."
  (pcase parsed-args
    ;; operators
    (`(:and . ,args)
     (cl-reduce (lambda (arg1 arg2)
                  `(,@arg1 "-and" ,@arg2))
                (cl-mapcar #'f3-parsed-to-command args)))
    (`(:or . ,args)
     (cl-reduce (lambda (arg1 arg2)
                  `(,@arg1 "-or" ,@arg2))
                (cl-mapcar #'f3-parsed-to-command args)))
    (`(:not ,thing) `("-not" ,@(f3-parsed-to-command thing)))
    (`(:paren ,thing) `("(" ,@(f3-parsed-to-command thing) ")"))
    ;; modes
    (`(:text ,thing) (f3-maybe-lowercase-generate "path" thing))
    (`(:regex ,thing) (f3-maybe-lowercase-generate "regex" thing))
    (`(:raw ,thing) thing)
    (`(:filetype ,thing) `("-type" ,thing))
    (`(:perm ,thing) `("-perm" ,thing))
    (_ (error (format "cannot comprehend arguments %S" parsed-args)))))

(defun f3-get-buffer-names ()
  (when f3-match-buffers
    (mapcar
     (lambda (buf) (cons (buffer-name buf) buf))
     (cl-remove-if-not #'buffer-file-name (buffer-list)))))

(defun f3-buffer-persistent-action (buf)
  (switch-to-buffer buf))

(defun f3-reduce-atom-and-cons (an-atom comb new-atom)
  (if an-atom (list comb new-atom an-atom) new-atom))

(defun f3-process-current-node (reduced atom comb left)
  (if (eq atom :right-paren)
      (let ((new-init (cl-second left))
            (new-left (nthcdr 2 left)))
        ;; if empty parens
        (if (eq (car new-init) :left-paren)
            ;; returns here
            (cons reduced new-left)
          (let* ((res (f3-parse-upto-left-paren-or-end
                       new-init new-left))
                 (new-reduced (car res))
                 (new-left (cdr res)))
            ;; returns here
            (cons (f3-reduce-atom-and-cons reduced comb new-reduced)
                  new-left))))
    ;; returns here
    (cons (f3-reduce-atom-and-cons reduced comb atom)
          (cdr left))))

(defun f3-parse-upto-left-paren-or-end (reduced left)
  (cl-loop
   for cur = (car left)
   for comb = (car cur)
   for atom = (cdr cur)
   while (and cur (not (eq comb :left-paren)))
   do (let ((res (f3-process-current-node reduced atom comb left)))
        (setq reduced (car res)
              left (cdr res)))
   finally return (cons reduced left)))

(defun f3-swallow-left-parens (reduced remaining)
  (cl-loop
   for res = (f3-parse-upto-left-paren-or-end reduced remaining)
   for new-reduced = (car res)
   for new-remaining = (cdr res)
   while remaining
   do (setq reduced new-reduced
            ;; skip :left-paren
            remaining (cdr new-remaining))
   finally return reduced))

(defun f3-get-ast ()
  (let ((current-pattern
         (unless (string-empty-p helm-pattern)
           (f3-pattern-to-parsed-arg helm-pattern))))
    (message "stack: %S, h-p: %s" f3-current-operator-stack helm-pattern)
    (f3-swallow-left-parens current-pattern f3-current-operator-stack)))

(defun f3-filter-buffer-candidates (cand)
  (cl-case f3-current-mode
    (:text (helm-mm-3-match cand (regexp-quote helm-pattern)))
    (:regex (helm-mm-3-match cand helm-pattern))
    (t nil)))

(defun f3-make-process ()
  (let ((final-pat (f3-get-ast)))
    (message "pat: %S" final-pat)
    (when final-pat
      (with-current-buffer f3-source-buffer
        ;; n.b.: `f3-async-filter-function' depends upon the "." literal
        (let* ((args `(,f3-find-program "." ,@(f3-parsed-to-command final-pat)))
               (default-directory f3-cached-dir)
               (err-proc (make-pipe-process
                          :name f3-err-proc-name
                          :buffer f3-err-buf-name
                          :command '("cat")))
               (real-proc
                (make-process
                 :name f3-proc-name
                 :buffer f3-buf-name
                 :command args
                 :stderr err-proc)))
          (set-process-filter
           err-proc
           (lambda (proc ev)
             (if (zerop (process-exit-status real-proc))
                 (with-current-buffer (process-buffer proc) (insert ev))
               (with-current-buffer (helm-buffer-get)
                 (erase-buffer)
                 ;; TODO: remove results from the async source if this happens
                 (let ((err-msg (propertize "find failed with error:"
                                            'face f3-err-msg-props)))
                   (insert (format "%s\n%s" err-msg ev)))))))
          (helm-attrset
           'name (format "%s: %s" f3-cached-dir (mapconcat #'identity args " "))
           f3-find-process-source)
          (message "default-directory: %s, args: %S, mode: %S, ast: %S, stack: %S"
                   default-directory args f3-current-mode final-pat f3-current-operator-stack)
          real-proc)))))

(defun f3-clear-opened-persistent-buffers ()
  (cl-mapc #'kill-buffer f3-currently-opened-persistent-buffers)
  (setq f3-currently-opened-persistent-buffers nil))

(defun f3-sync-action (buf)
  (setq f3-last-selected-candidate buf)
  (switch-to-buffer buf))

(defun f3-async-filter-function (cand)
  "Remove leading './' from candidate CAND."
  (replace-regexp-in-string "\\`\\./" "" cand))

(defun f3-async-display-to-real (cand &optional persistent)
  (with-current-buffer f3-source-buffer
    (let* ((default-directory f3-cached-dir)
           (buf (find-buffer-visiting cand)))
      (unless buf
        (setq buf (find-file cand))
        (when persistent
          (push buf f3-currently-opened-persistent-buffers)))
      buf)))

(defun f3-async-persistent-action (cand)
  (f3-buffer-persistent-action (f3-async-display-to-real cand t)))

(defun f3-async-action (cand)
  (f3-sync-action (f3-async-display-to-real cand)))

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

(defun f3-choose-dir ()
  (with-current-buffer (or f3-source-buffer (current-buffer))
    (or f3-cached-dir
        (setq f3-cached-dir
              (if (functionp f3-default-directory)
                  (f3-default-directory (current-buffer))
                (cl-case f3-default-directory
                  (project (f3-use-project-dir))
                  (choose (f3-explicitly-choose-dir))
                  (t default-directory)))))))

(defmacro f3-run-after-exit (&rest body)
  `(helm-run-after-exit (lambda () ,@body)))

(defun f3-choose-dir-and-rerun (dir-fun)
  (lambda ()
    (interactive)
    (with-current-buffer f3-source-buffer
      (f3-run-after-exit
       (let ((f3-cached-dir (funcall dir-fun f3-cached-dir)))
         (f3-do helm-pattern))))))

(defun f3-set-mode-and-rerun (mode)
  (lambda ()
    (interactive)
    (f3-run-after-exit
     (let ((f3-current-mode mode))
       (f3-do helm-pattern)))))

;;; TODO: make a version of find which ignores .gitignore/.agignore/etc
;;; TODO: along with run-shell-command/run-lisp on results, also allow user to
;;; dump to a REAL dired buffer
;;; TODO: add "use previous find command" command to use
;;; run-{lisp,shell}{,-interactively} or to just list files
;;; TODO: run-lisp-interactively could read in an expression which resolves to a
;;; function or lambda (accepting either each file individually or the entire
;;; list) and attach itself to a process filter or sentinel to get nonblocking
;;; results that fill in as you type

(defun f3-toggle-complement ()
  (interactive)
  (f3-run-after-exit
   (let ((f3-current-complement (not f3-current-complement)))
     (f3-do helm-pattern))))

(defun f3-attach-union ()
  (interactive)
  (f3-run-after-exit
   (let ((f3-current-operator-stack
          (cons (cons :or (f3-pattern-to-parsed-arg helm-pattern))
                f3-current-operator-stack))
         (f3-match-buffers nil))
     (f3-do))))

(defun f3-attach-intersection ()
  (interactive)
  (f3-run-after-exit
   (let ((f3-current-operator-stack
          (cons (cons :and (f3-pattern-to-parsed-arg helm-pattern))
                f3-current-operator-stack))
         (f3-match-buffers nil))
     (f3-do))))

(defun f3-left-paren ()
  (interactive)
  (f3-run-after-exit
   (let ((f3-current-operator-stack
          (cons (list :left-paren) f3-current-operator-stack))
         (f3-match-buffers nil))
     (f3-do helm-pattern))))

(defun f3-right-paren (comb)
  (lambda ()
    (interactive)
    (f3-run-after-exit
     (let ((f3-current-operator-stack
            (append (list (cons comb :right-paren))
                    (unless (string= helm-pattern "")
                      (f3-pattern-to-parsed-arg helm-pattern))
                    f3-current-operator-stack))
           (f3-match-buffers nil))
       (f3-do)))))

(defun f3-do (&optional initial-input)
  (let ((last-cand
         (if (buffer-live-p f3-last-selected-candidate)
             (buffer-name f3-last-selected-candidate)
           (setq f3-last-selected-candidate nil)))
        (prompt (concat
                 (if f3-current-complement "(not) " "")
                 (substring (symbol-name f3-current-mode) 1)
                 ": ")))
    (helm :sources '(f3-find-process-source f3-buffer-source)
          :buffer f3-helm-buffer-name
          :input (or initial-input "")
          :preselect last-cand
          :prompt prompt
          :keymap f3-map)))


;; Keymap
(defconst f3-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-o") (f3-choose-dir-and-rerun #'f3-use-project-dir))
    (define-key map (kbd "M-i") (f3-choose-dir-and-rerun #'f3-use-file-dir))
    (define-key map (kbd "M-c")
      (f3-choose-dir-and-rerun #'f3-explicitly-choose-dir))
    (define-key map (kbd "M-j") (f3-choose-dir-and-rerun #'f3-up-dir))
    (define-key map (kbd "M-t") (f3-set-mode-and-rerun :text))
    (define-key map (kbd "M-r") (f3-set-mode-and-rerun :regex))
    (define-key map (kbd "M-f") (f3-set-mode-and-rerun :raw))
    (define-key map (kbd "M-d") (f3-set-mode-and-rerun :filetype))
    (define-key map (kbd "M-p") (f3-set-mode-and-rerun :perm))
    (define-key map (kbd "M-q") #'f3-toggle-complement)
    (define-key map (kbd "M-+") #'f3-attach-union)
    (define-key map (kbd "M-*") #'f3-attach-intersection)
    (define-key map (kbd "M-(") #'f3-left-paren)
    (define-key map (kbd "M-) M-+") (f3-right-paren :or))
    (define-key map (kbd "M-) M-*") (f3-right-paren :and))
    map)
  "Keymap for `f3'.")


;; Autoloaded functions
;;;###autoload
(defun f3 (start-dir)
  (interactive (list (f3-choose-dir)))
  (let ((f3-source-buffer (current-buffer))
        (f3-current-mode f3-default-mode)
        (f3-current-complement nil)
        (f3-current-operator-stack nil)
        (f3-current-redo-stack nil)
        (f3-match-buffers t))
    (f3-do)))

(provide 'f3)
;;; f3.el ends here
