;;; keys --- global key bindings  ;; -*- lexical-binding: t; -*-

;;; Commentary:
;;;; Remember that you can run `M-x describe-personal-keybindings' to
;;;; get a list of what's bound to what

;;; Code:
;;;; bindings that don't have another obvious place to put them go here
(bind-key "C-#"         #'comment-or-uncomment-region)

(bind-key "C-c b"       #'bury-buffer)
(bind-key* "C-c C-b"     #'bury-buffer) ; fuck you emacs-lisp-mode
(bind-key "C-c C-SPC"   #'just-one-space)

(bind-key "C-s"         #'isearch-forward)

(bind-key "M-o"         #'other-window)
(bind-key "M-i"         #'imenu)
(bind-key "C-c l"        #'org-store-link)
(bind-key "C-c a"        #'org-agenda)
(bind-key "C-c c"        #'org-capture)

(add-hook 'org-mode-hook
          (lambda ()
            (keymap-set org-mode-map "M-s j"
                        'consult-org-heading)))

(provide 'keys)
;;; keys.el ends here
