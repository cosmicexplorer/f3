;;; f3.el --- The Fantastic File Finder: a helm interface for searching files really fast -*- lexical-binding: t -*-

;; Author: Danny McClanahan <danieldmcclanahan@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "24") (helm "1.9.6") (cl-lib "0.5"))
;; Keywords: find, file, files, helm, fast, finder


;;; Commentary:

;; Focuses on two use cases:
;; 1. Finding a file in a project really fast.
;; 2. Finding some complex set of files and performing some action on them.

;; Further documentation available at https://github.com/cosmicexplorer/f3


;; Further work:
;; TODO: show state of undo/redo in some readable way
;; TODO: fix highlighting of results in helm and highlighting of previews


;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'helm-multi-match)
(require 'subr-x)
(require 'find-dired)


;; Customization
(defgroup f3 nil "Group for `f3' customizations.")

(defcustom f3-default-mode :text
  "Default input mode for `f3' patterns."
  :type 'symbol
  :safe #'f3--always-valid
  :group 'f3)

(defcustom f3-find-program "find"
  "Default command to find files with using `f3'."
  :type 'string
  :safe #'f3--always-valid
  :group 'f3)

(defcustom f3-default-directory 'default
  "Default directory to set as pwd when running `f3'. 'project for the project
directory, 'choose to choose every time, and nil (or anything else) to choose
the current directory of the buffer in which `f3' is run. See the source of
`f3--choose-dir' for details. Can be a function accepting the current directory
and returning a directory path."
  :type '(choice symbol function string)
  :safe #'f3--validate-default-directory
  :group 'f3)

(defcustom f3-before-args '("-not" "-ipath" "*.git*")
  "Arguments to be placed before all calls to find."
  :type '(repeat string)
  :safe #'f3--always-valid
  :group 'f3)

(defcustom f3-project-base-file-regexen '("\\`\\.git\\'")
  "Regular expressions denoting files which are the \"base\" of a project."
  :type '(repeat string)
  :safe #'f3--always-valid
  :group 'f3)


;; Constants
(defconst f3--input-modes
  '((:text . f3--create-text-pattern)
    (:regex . f3--create-regex-pattern)
    (:raw . f3--create-raw-pattern)
    (:filetype . f3--create-filetype-pattern)
    (:perm . f3--create-perm-pattern))
  "Modes which interpret the current `helm-pattern' differently.")

(defconst f3--combinators '(:and :or))

(defconst f3--helm-buffer-name "*f3*")
(defconst f3--proc-name "*f3--find*")
(defconst f3--buf-name "*f3--find-output*")

(defconst f3--candidate-limit 2000)

(defconst f3--err-proc-name "*f3--err-proc*")
(defconst f3--err-buf-name "*f3--errors*")
(defconst f3--err-msg-props '(font-lock-warning-face :height 2.0))

(defconst f3--start-anchors '("\\`" "^"))
(defconst f3--end-anchors '("\\'" "$"))
(defconst f3--shell-wildcards '("*"))


;; Global variables
(defvar f3--current-mode)

(defvar f3--current-complement)

(defvar f3--match-buffers t
  "Whether to match buffers as well as async find results. Starts on, turned off
within a session after a combinator is used.")

(defvar f3--buffer-source
  (helm-build-sync-source "open buffers"
    :candidates #'f3--get-buffer-names
    :match-strict #'f3--filter-buffer-candidates
    :candidate-number-limit f3--candidate-limit
    :action (helm-make-actions "Visit" #'f3--sync-action)
    :persistent-action #'f3--buffer-persistent-action)
  "Source searching currently open buffer names for results.")

(defvar f3--currently-opened-persistent-buffers nil
  "The buffers opened through a persistent action within an `f3' session. These
are killed at the end of a session.")

(defvar f3--find-process-source
  (helm-build-async-source "find"
    :candidates-process #'f3--make-process
    :candidate-number-limit f3--candidate-limit
    :action (helm-make-actions "Visit" #'f3--async-action)
    :persistent-action #'f3--async-persistent-action
    :filter-one-by-one #'f3--async-filter-function
    :cleanup #'f3--cleanup)
  "Source searching files within a given directory using the find command.")

(defvar f3--last-selected-candidate nil
  "Buffer which was last selected through `f3'.")

(defvar f3--source-buffer nil
  "Buffer which is current when `f3' is invoked.")

(defvar f3--current-operator-stack nil
  "Current stream of parsed operators.")

(defvar f3--current-redo-stack nil
  "Current stream of parsed operators along with any hanging after undos.")

(defvar f3--current-mindepth nil)
(defvar f3--current-maxdepth nil)

(defvar f3--temp-pattern nil)

(defvar f3--prev-stack-and-cur nil)


;; Buffer-local variables
(defvar-local f3--cached-dir nil)


;; Functions
(defun f3--validate-default-directory (val)
  (or (stringp val) (symbolp val)))

(defun f3--always-valid (_) t)

(defun f3--wildcard-unless-meta (pat start-anchors end-anchors insert)
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

(defun f3--shell-expansion-unless-wildcard (pat)
  (f3--wildcard-unless-meta pat f3--shell-wildcards f3--shell-wildcards "*"))

(defun f3--create-text-pattern (pat)
  (let ((texts (helm-mm-split-pattern pat)))
    (cl-reduce
     (lambda (parsed1 parsed2) `(:and ,parsed1 ,parsed2))
     (cl-mapcar
      (lambda (pat)
        `(:text ,pat))
      texts))))

(defun f3--dot-star-unless-anchor (pat)
  (f3--wildcard-unless-meta pat f3--start-anchors f3--end-anchors ".*"))

(defun f3--create-regex-pattern (pat)
  (let ((texts (helm-mm-3-get-patterns pat)))
    (cl-reduce
     (lambda (parsed1 parsed2) `(:and ,parsed1 ,parsed2))
     (cl-mapcar
      (lambda (pat)
        (cl-destructuring-bind (pred . reg) pat
          (if (eq pred 'identity)
              `(:regex ,real-reg)
            `(:not (:regex ,real-reg)))))
      texts))))

(defun f3--create-raw-pattern (pat)
  "Assumes correctness of pattern PAT."
  (let ((split-pattern
         (with-temp-buffer
           (push-mark)
           (insert pat)
           (car (shell--parse-pcomplete-arguments)))))
    `(:raw ,split-pattern)))

(defun f3--create-filetype-pattern (pat)
  `(:filetype ,pat))

(defun f3--create-perm-pattern (pat)
  `(:perm ,pat))

(defun f3--do-complement (ast do-complement)
  (if do-complement `(:not ,ast) ast))

(defun f3--pattern-to-parsed-arg (pattern)
  (let ((process-input-fn (cdr (assoc f3--current-mode f3--input-modes))))
    (if process-input-fn
        (f3--do-complement (funcall process-input-fn pattern)
                          f3--current-complement)
      (error (format "%s '%S'" "invalid mode" f3--current-mode)))))

(defun f3--maybe-lowercase-generate (base pat)
  (let ((case-fold-search nil))
    (if (string-match-p "[[:upper:]]" pat)
        (list (format "-%s" base) pat)
      (list (format "-i%s" base) pat))))

;;; TODO: fix helm's automatic highlighting of results; maybe use some logic in
;;; `f3--filter-buffer-candidates'?
(defun f3--parsed-to-command (parsed-args)
  "Transform PARSED-ARGS to a raw find command."
  (pcase parsed-args
    ;; pass-through
    (`(:atom ,thing) (f3--parsed-to-command thing))
    ;; operators
    (`(:and . ,args)
     (cl-reduce (lambda (arg1 arg2)
                  `(,@arg1 "-and" ,@arg2))
                (cl-mapcar #'f3--parsed-to-command args)))
    (`(:or . ,args)
     (cl-reduce (lambda (arg1 arg2)
                  `(,@arg1 "-or" ,@arg2))
                (cl-mapcar #'f3--parsed-to-command args)))
    (`(:not ,thing) `("-not" ,@(f3--parsed-to-command thing)))
    (`(:paren ,thing) `("(" ,@(f3--parsed-to-command thing) ")"))
    ;; modes
    (`(:text ,thing) (f3--maybe-lowercase-generate
                      "path" (f3--shell-expansion-unless-wildcard thing)))
    (`(:regex ,thing) (f3--maybe-lowercase-generate
                       "regex" (f3--dot-star-unless-anchor thing)))
    (`(:raw ,thing) thing)
    (`(:filetype ,thing) `("-type" ,thing))
    (`(:perm ,thing) `("-perm" ,thing))
    (_ (error (format "cannot comprehend arguments %S" parsed-args)))))

(defun f3--get-buffer-names ()
  (when f3--match-buffers
    (mapcar
     (lambda (buf) (cons (buffer-name buf) buf))
     (cl-remove-if-not #'buffer-file-name (buffer-list)))))

(defun f3--buffer-persistent-action (buf)
  (switch-to-buffer buf))

(defun f3--reduce-atom-and-cons (an-atom comb new-atom)
  (if an-atom (list comb new-atom an-atom) new-atom))

(defun f3--maybe-add-paren (reduced)
  (if (eq (car reduced) :paren) reduced
    `(:paren ,reduced)))

(defun f3--process-current-node (reduced atom comb left)
  (if (eq atom :right-paren)
      (let ((new-init (cl-second left))
            (new-left (cddr left)))
        ;; if empty parens
        (if (eq (car new-init) :left-paren)
            ;; returns here
            (cons reduced new-left)
          (let* ((res (f3--parse-upto-left-paren-or-end
                       new-init new-left))
                 (new-reduced (f3--maybe-add-paren (car res)))
                 (new-left (cddr res)))
            ;; returns here
            (cons (f3--reduce-atom-and-cons reduced comb new-reduced)
                  new-left))))
    ;; returns here
    (cons (f3--reduce-atom-and-cons reduced comb atom)
          (cdr left))))

(defun f3--parse-upto-left-paren-or-end (reduced left)
  (cl-loop
   for cur = (car left)
   for comb = (car cur)
   for atom = (cdr cur)
   while (and cur (not (eq comb :left-paren)))
   do (let ((res (f3--process-current-node reduced atom comb left)))
        (setq reduced (car res)
              left (cdr res)))
   finally return (let ((real-reduced
                         (if (and reduced (eq comb :left-paren))
                             (if (eq (car atom) :not)
                                 `(:not (:paren ,reduced))
                               `(:paren ,reduced))
                           reduced)))
                    (cons real-reduced left))))

(defun f3--swallow-left-parens (reduced remaining)
  (cl-loop
   for res = (f3--parse-upto-left-paren-or-end reduced remaining)
   for new-reduced = (car res)
   for new-remaining = (cdr res)
   while remaining
   do (setq reduced new-reduced
            ;; skip :left-paren
            remaining (cdr new-remaining))
   finally return reduced))

(defun f3--get-ast ()
  (let ((current-pattern
         (unless (string-empty-p helm-pattern)
           (f3--pattern-to-parsed-arg helm-pattern))))
    (f3--swallow-left-parens current-pattern f3--current-operator-stack)))

(defun f3--filter-buffer-candidates (cand)
  (cl-case f3--current-mode
    (:text (helm-mm-3-match cand (regexp-quote helm-pattern)))
    (:regex (helm-mm-3-match cand helm-pattern))
    (t nil)))

(defun f3--add-depths-to-args (args)
  (when f3--current-maxdepth
    (setq args `("-maxdepth" ,(number-to-string f3--current-maxdepth) ,@args)))
  (when f3--current-mindepth
    (setq args `("-mindepth" ,(number-to-string f3--current-mindepth) ,@args)))
  args)

(defun f3--get-find-args ()
  (let ((final-pat (f3--get-ast)))
    (when final-pat
      (let* ((args-minus-depth (f3--parsed-to-command final-pat))
             (depth-args (f3--add-depths-to-args args-minus-depth)))
        (with-current-buffer f3--source-buffer
          (if f3-before-args
              `("(" ,@f3-before-args ")" "-and" "(" ,@depth-args ")")
            depth-args))))))

(defun f3--make-process ()
  (let ((args (f3--get-find-args)))
    (when args
      (with-current-buffer f3--source-buffer
        ;; n.b.: `f3--async-filter-function' depends upon the "." literal
        (let* ((all-args `(,f3-find-program "." ,@args))
               (default-directory f3--cached-dir)
               (err-proc
                (progn
                  (when (process-live-p f3--err-proc-name)
                    (kill-process f3--err-proc-name))
                  (make-pipe-process
                   :name f3--err-proc-name
                   :buffer f3--err-buf-name
                   :command '("cat"))))
               (real-proc
                (make-process
                 :name f3--proc-name
                 :buffer f3--buf-name
                 :command all-args
                 :stderr err-proc)))
          (set-process-filter
           err-proc
           (lambda (proc ev)
             (if (zerop (process-exit-status real-proc))
                 (with-current-buffer (process-buffer proc) (insert ev))
               (with-current-buffer (helm-buffer-get)
                 (erase-buffer)
                 (let ((err-msg (propertize "find failed with error:"
                                            'face f3--err-msg-props)))
                   (insert (format "%s\n%s" err-msg ev)))
                 (when (process-live-p real-proc) (kill-process real-proc))))))
          (helm-attrset
           'name
           (format "%s: %s" f3--cached-dir (mapconcat #'identity all-args " "))
           f3--find-process-source)
          real-proc)))))

(defun f3--cleanup ()
  (f3--save-previous-command)
  (f3--clear-opened-persistent-buffers))

(defun f3--save-previous-command ()
  (setq f3--prev-stack-and-cur
        (list f3--current-redo-stack
              (f3--pattern-to-parsed-arg helm-pattern))))

(defun f3--clear-opened-persistent-buffers ()
  (cl-mapc #'kill-buffer f3--currently-opened-persistent-buffers)
  (setq f3--currently-opened-persistent-buffers nil))

(defun f3--sync-action (buf)
  (setq f3--last-selected-candidate buf)
  (switch-to-buffer buf))

(defun f3--async-filter-function (cand)
  "Remove leading './' from candidate CAND."
  (replace-regexp-in-string "\\`\\./" "" cand))

(defun f3--async-display-to-real (cand &optional persistent)
  (with-current-buffer f3--source-buffer
    (let* ((default-directory f3--cached-dir)
           (buf (find-buffer-visiting cand)))
      (unless buf
        (setq buf (find-file cand))
        (when persistent
          (push buf f3--currently-opened-persistent-buffers)))
      buf)))

(defun f3--async-persistent-action (cand)
  (f3--buffer-persistent-action (f3--async-display-to-real cand t)))

(defun f3--async-action (cand)
  (f3--sync-action (f3--async-display-to-real cand)))

(defun f3--find-base-files (dir)
  (cl-some
   (lambda (file)
     (cl-some (lambda (regexp)
                (string-match-p regexp file))
              f3-project-base-file-regexen))
   (directory-files dir)))

(defun f3--string-starts-with (str start-str)
  (string-match-p (concat "\\`" (regexp-quote start-str)) str))

(defun f3--use-project-dir (from)
  (cl-loop for dir = (expand-file-name from)
           then (expand-file-name (concat dir "/.."))
           with found = nil
           until (f3--string-starts-with (expand-file-name "~") dir)
           do (setq found (f3--find-base-files dir))
           until found
           finally return (if found dir from)))

(defun f3--explicitly-choose-dir (from)
  (expand-file-name (read-directory-name "Directory to search: " from nil t)))

(defun f3--up-dir (from)
  (expand-file-name (format "%s/../" from)))

(defun f3--choose-dir ()
  (with-current-buffer (or f3--source-buffer (current-buffer))
    (or f3--cached-dir
        (setq f3--cached-dir
              (cond ((functionp f3-default-directory)
                     (funcall f3-default-directory default-directory))
                    ((stringp f3-default-directory) f3-default-directory)
                    (t
                     (cl-case f3-default-directory
                       (project (f3--use-project-dir default-directory))
                       (choose (f3--explicitly-choose-dir default-directory))
                       (t default-directory))))))))

(defmacro f3--run-after-exit (&rest body)
  `(helm-run-after-exit (lambda () ,@body)))

(defun f3--choose-dir-and-rerun (dir-fun)
  (lambda ()
    (interactive)
    (let ((pat helm-pattern))
      (with-current-buffer f3--source-buffer
        (f3--run-after-exit
         (let ((f3--cached-dir (funcall dir-fun f3--cached-dir)))
           (f3--do pat t)))))))

(defun f3--set-mode-and-rerun (mode)
  (lambda ()
    (interactive)
    (f3--run-after-exit
     (let ((f3--current-mode mode))
       (f3--do helm-pattern t)))))

(defun f3--toggle-complement ()
  (interactive)
  (f3--run-after-exit
   (let ((f3--current-complement (not f3--current-complement))
         (f3--match-buffers nil))
     (f3--do helm-pattern t))))

(defun f3--attach-union ()
  (interactive)
  (f3--run-after-exit
   (let* ((f3--current-operator-stack
          (cons (cons :or (f3--pattern-to-parsed-arg helm-pattern))
                f3--current-operator-stack))
          (f3--current-redo-stack f3--current-operator-stack)
          (f3--temp-pattern nil)
          (f3--match-buffers nil))
     (f3--do))))

(defun f3--attach-intersection ()
  (interactive)
  (f3--run-after-exit
   (let* ((f3--current-operator-stack
           (cons (cons :and (f3--pattern-to-parsed-arg helm-pattern))
                 f3--current-operator-stack))
          (f3--current-redo-stack f3--current-operator-stack)
          (f3--temp-pattern nil)
          (f3--match-buffers nil))
     (f3--do))))

(defun f3--left-paren (pfx)
  (interactive "P")
  (f3--run-after-exit
   (let* ((f3--current-operator-stack
           (cons (list :left-paren (when pfx :not)) f3--current-operator-stack))
          (f3--current-redo-stack f3--current-operator-stack)
          (f3--temp-pattern nil)
          (f3--match-buffers nil))
     (f3--do helm-pattern t))))

(defun f3--right-paren (comb)
  (lambda ()
    (interactive)
    (f3--run-after-exit
     (let* ((f3--current-operator-stack
             (append (list (cons comb :right-paren))
                     (unless (string= helm-pattern "")
                       (list
                        (list :atom (f3--pattern-to-parsed-arg helm-pattern))))
                     f3--current-operator-stack))
            (f3--temp-pattern nil)
            (f3--current-redo-stack f3--current-operator-stack)
            (f3--match-buffers nil))
       (f3--do)))))

(defun f3--set-mindepth ()
  (interactive)
  (f3--run-after-exit
   (let* ((res (read-number "new mindepth: " (or f3--current-mindepth -1)))
          (f3--current-mindepth (if (< res 0) nil res))
          (f3--match-buffers nil))
     (f3--do helm-pattern t))))

(defun f3--set-maxdepth ()
  (interactive)
  (f3--run-after-exit
   (let* ((res (read-number "new maxdepth: " (or f3--current-maxdepth -1)))
          (f3--current-maxdepth (if (< res 0) nil res))
          (f3--match-buffers nil))
     (f3--do helm-pattern t))))

(defun f3--find-next-stack-entry (start)
  (cl-loop with head = (f3--find-previous-text-pattern start)
           with head2 = (f3--find-previous-text-pattern (cdr head))
           while (and head2
                      (not (cl-find (car head2) f3--current-operator-stack)))
           do (setq head head2
                    head2 (f3--find-previous-text-pattern (cdr head2)))
           finally return head))

(defun f3--edit-current-stack-entry ()
  (let ((new-stack (f3--find-next-stack-entry f3--current-redo-stack)))
    (setf (car new-stack)
          (if (memq (caar new-stack) f3--combinators)
              (cons (caar new-stack) (f3--pattern-to-parsed-arg helm-pattern))
            (list (caar new-stack)
                  (f3--pattern-to-parsed-arg helm-pattern))))))

(defun f3--find-previous-text-pattern (start)
  (cl-loop for head = start then (cdr head)
           for cur = (car head)
           while head until (not (or (eq (car cur) :left-paren)
                                     (eq (cdr cur) :right-paren)))
           finally return head))

(defun f3--restore-from-previous-command ()
  (interactive)
  (when f3--prev-stack-and-cur
    (cl-destructuring-bind (redo-stack cur-pat)
        f3--prev-stack-and-cur
      (f3--run-after-exit
       (let ((f3--current-redo-stack redo-stack)
             (f3--current-operator-stack redo-stack))
         (f3--set-current-pattern-from-link cur-pat))))))

(defun f3--set-current-pattern-from-link (link &optional comp)
  (let ((f3--current-complement comp))
    (if (memq (car link) f3--combinators)
        (f3--set-current-pattern-from-link (cdr link))
      (cl-case (car link)
        (:not (f3--set-current-pattern-from-link (cl-second link) t))
        (:atom (f3--set-current-pattern-from-link (cl-second link) comp))
        (t (let ((f3--current-mode (car link)))
             (f3--do (cl-second link) t)))))))

(defun f3--do-undo ()
  (let ((new-head (f3--find-previous-text-pattern f3--current-operator-stack)))
    (let ((f3--current-operator-stack (cdr new-head)))
      (if new-head
          (f3--set-current-pattern-from-link (car new-head))
        (f3--do helm-pattern t)))))

(defun f3--undo ()
  (interactive)
  (f3--run-after-exit
   (if (eq f3--current-operator-stack f3--current-redo-stack)
       (let ((f3--temp-pattern
              (list (f3--pattern-to-parsed-arg helm-pattern)
                    f3--current-operator-stack)))
         (f3--do-undo))
     (f3--edit-current-stack-entry)
     (f3--do-undo))))

(defun f3--find-next-text-pattern (start)
  (cl-loop with head = (f3--find-previous-text-pattern start)
           with head2 = (f3--find-previous-text-pattern (cdr head))
           with head3 = (f3--find-previous-text-pattern (cdr head2))
           while (and head3
                      (not (cl-find (car head3) f3--current-operator-stack)))
           do (setq head head2
                    head2 head3
                    head3 (f3--find-previous-text-pattern (cdr head3)))
           finally return head))

(defun f3--get-twice-previous-text-pattern (start)
  (f3--find-previous-text-pattern
   (cdr (f3--find-previous-text-pattern start))))

(defun f3--redo ()
  (interactive)
  (f3--run-after-exit
   (let* ((new-head
           (and (not (eq f3--current-operator-stack
                         f3--current-redo-stack))
                (f3--find-next-text-pattern f3--current-redo-stack)))
          (is-ready-for-temp-pattern
           (and new-head
                (and f3--temp-pattern
                     (eq f3--current-operator-stack
                         (f3--get-twice-previous-text-pattern
                          f3--current-redo-stack)))))
          (do-edit-stack-entry (not (null f3--current-operator-stack)))
          (f3--current-operator-stack
           (if new-head (cdr new-head) f3--current-operator-stack)))
     (if new-head
         (progn
           (when do-edit-stack-entry (f3--edit-current-stack-entry))
           (if is-ready-for-temp-pattern
               (cl-destructuring-bind (pat f3--current-operator-stack)
                   f3--temp-pattern
                 (f3--set-current-pattern-from-link pat))
             (f3--set-current-pattern-from-link (car new-head))))
       (f3--do helm-pattern t)))))

(defmacro f3--clear-session-variables (&rest body)
  `(let ((f3--current-mode f3-default-mode)
         f3--current-complement
         f3--current-operator-stack
         f3--current-redo-stack
         (f3--match-buffers t)
         f3--current-mindepth
         f3--current-maxdepth
         f3--temp-pattern
         f3--shell-command
         f3--interactive-p)
     ,@body))

(defun f3--bounce-to-raw ()
  (interactive)
  (let ((raw-cmd (mapconcat #'identity (f3--get-find-args) " ")))
    (f3--clear-session-variables
     (f3--run-after-exit
      (let ((f3--current-mode :raw))
        (f3--do raw-cmd))))))

(defun f3--surround-with-quotes (str) (format "'%s'" str))

(defun f3--dump-to-dired ()
  (interactive)
  (let ((raw-cmd (mapconcat #'f3--surround-with-quotes (f3--get-find-args) " "))
        (dir default-directory))
    (f3--run-after-exit
     (find-dired dir raw-cmd))))

(defun f3--do (&optional initial-input preserve-complement)
  (let* ((last-cand
          (if (buffer-live-p f3--last-selected-candidate)
              (buffer-name f3--last-selected-candidate)
            (setq f3--last-selected-candidate nil)))
         (f3--current-complement
          (if preserve-complement f3--current-complement nil))
         (prompt (concat
                  (if f3--current-complement "(not) " "")
                  (substring (symbol-name f3--current-mode) 1)
                  ": ")))
    (helm :sources '(f3--find-process-source f3--buffer-source)
          :buffer f3--helm-buffer-name
          :input (or initial-input "")
          :preselect last-cand
          :prompt prompt
          :keymap f3-map)))


;; Keymaps
(defconst f3-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-o")
      (f3--choose-dir-and-rerun #'f3--use-project-dir))
    (define-key map (kbd "M-i") (f3--choose-dir-and-rerun #'f3--use-file-dir))
    (define-key map (kbd "M-c")
      (f3--choose-dir-and-rerun #'f3--explicitly-choose-dir))
    (define-key map (kbd "M-j") (f3--choose-dir-and-rerun #'f3--up-dir))
    (define-key map (kbd "M-t") (f3--set-mode-and-rerun :text))
    (define-key map (kbd "M-x") (f3--set-mode-and-rerun :regex))
    (define-key map (kbd "M-r") (f3--set-mode-and-rerun :raw))
    (define-key map (kbd "M-f") (f3--set-mode-and-rerun :filetype))
    (define-key map (kbd "M-p") (f3--set-mode-and-rerun :perm))
    (define-key map (kbd "M-q") #'f3--toggle-complement)
    (define-key map (kbd "M-+") #'f3--attach-union)
    (define-key map (kbd "M-*") #'f3--attach-intersection)
    (define-key map (kbd "M-(") #'f3--left-paren)
    (define-key map (kbd "M-) M-+") (f3--right-paren :or))
    (define-key map (kbd "M-) M-*") (f3--right-paren :and))
    (define-key map (kbd "M-<") #'f3--set-mindepth)
    (define-key map (kbd "M->") #'f3--set-maxdepth)
    (define-key map (kbd "M-u") #'f3--undo)
    (define-key map (kbd "M-U") #'f3--redo)
    (define-key map (kbd "M-b") #'f3--bounce-to-raw)
    (define-key map (kbd "M-d") #'f3--dump-to-dired)
    (define-key map (kbd "M-R") #'f3--restore-from-previous-command)
    map)
  "Keymap for `f3'.")


;; Autoloaded functions
;;;###autoload
(defun f3 (start-dir)
  (interactive (list (f3--choose-dir)))
  (let ((f3--source-buffer (current-buffer)))
    (f3--clear-session-variables (f3--do))))

(provide 'f3)
;;; f3.el ends here
