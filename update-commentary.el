#!/bin/sh
:;exec emacs -batch -l "$0" -- "$@"

(defconst search-regexp "\\(;;; Commentary:\\)\\(?:[[:ascii:]]*?\\)\\(;; End Commentary\\)")
(with-current-buffer (find-file "f3.el")
  (re-search-forward search-regexp)
  (replace-match "\\1\n\\2")
  (save-buffer)
  (kill-buffer))

(let ((res (shell-command-to-string "./create-markdown.coffee")))
  (unless (string= res "")
    (message "%s" res)))

;; Local Variables:
;; mode: emacs-lisp
;; End:
