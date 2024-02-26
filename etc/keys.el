;;; keys --- global key bindings  ;; -*- lexical-binding: t; -*-

;;; Commentary:
;;;; Remember that you can run `M-x describe-personal-keybindings' to
;;;; get a list of what's bound to what

;;; Code:
;;;; bindings that don't have another obvious place to put them go here
(bind-key  "C-#"         #'comment-or-uncomment-region)

(bind-key  "C-c b"       #'bury-buffer)
(bind-key* "C-c C-b"     #'bury-buffer) ; fuck you emacs-lisp-mode
(bind-key  "C-c C-SPC"   #'just-one-space)

(bind-key  "C-s"         #'isearch-forward)

(bind-key  "M-o"         #'other-window)
(bind-key  "M-i"         #'imenu)


(provide 'keys)
;;; keys.el ends here
