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
(bind-key "C-r"         #'isearch-backward)

(bind-key "M-o"         #'other-window)
(bind-key "M-i"         #'imenu)
(bind-key "C-c l"        #'org-store-link)
(bind-key "C-c a"        #'org-agenda)
(bind-key "C-c c"        #'org-capture)
(bind-key "M-f" #'forward-to-word)

(use-package evil
  :ensure t
  :defines
  evil-want-C-w-delete
  evil-want-keybinding
  evil-want-integration
  evil-want-C-w-in-emacs-state
  :functions
  evil-global-set-key
  evil-mode
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-want-C-w-delete nil)
  (setq evil-want-C-w-in-emacs-state nil)
  :config
  (evil-global-set-key 'normal (kbd "C-r") 'isearch-backward)
  (evil-global-set-key 'insert (kbd "C-r") 'isearch-backward)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :functions
  evil-collection-init
  :ensure t
  :config
  (evil-collection-init))


(add-hook 'org-mode-hook
          (lambda ()
            (keymap-set org-mode-map "M-s j"
                        'consult-org-heading)))

(provide 'keys)
;;; keys.el ends here
