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
(bind-key "M-f" #'forward-to-word)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(global-set-key (kbd "M-e") 'consult-buffer)
(global-set-key (kbd "M-O") 'find-file)
(global-set-key (kbd "M-w") 'kill-buffer)
(global-set-key (kbd "C-\\") 'split-window-right)


(use-package evil-nerd-commenter
  :bind ("s-/" . evilnc-comment-or-uncomment-lines))


(use-package smartparens
  :ensure t
  :init (require 'smartparens-config)
  :config
  (progn

    (define-key sp-keymap (kbd "M-{") 'sp-wrap-curly)
    (define-key sp-keymap (kbd "M-[") 'sp-wrap-square)
    (define-key sp-keymap (kbd "M-(") 'sp-wrap-round)
    (define-key sp-keymap (kbd "M-J") 'sp-forward-barf-sexp)
    (define-key sp-keymap (kbd "M-K") 'sp-forward-slurp-sexp)

    (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
    (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

    (define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
    (define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
    (define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
    (define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)

    (define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
    (define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
    (define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
    (define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

    (define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
    (define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)
    (define-key sp-keymap (kbd "C-k") 'sp-kill-sexp)
    (define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)
    (define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
    (define-key sp-keymap (kbd "C-S-<right>") 'sp-forward-slurp-sexp)
    (define-key sp-keymap (kbd "C-S-<left>") 'sp-forward-barf-sexp)
    (define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
    (define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)
    (define-key sp-keymap (kbd "C-M-S-<backspace>") 'sp-splice-sexp-killing-forward)

    (define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
    (define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)

    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

;;; markdown-mode
    (sp-with-modes '(markdown-mode gfm-mode rst-mode)
      (sp-local-pair "*" "*" :bind "C-*")
      (sp-local-tag "2" "**" "**")
      (sp-local-tag "s" "```scheme" "```")
      (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

;;; tex-mode latex-mode
    (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
      (sp-local-tag "i" "\"<" "\">"))

;;; html-mode
    (sp-with-modes '(html-mode sgml-mode)
      (sp-local-pair "<" ">"))

;;; lisp modes
    (sp-with-modes sp--lisp-modes
      (sp-local-pair "(" nil :bind "C-("))))

(add-hook 'org-mode-hook
          (lambda ()
            (keymap-set org-mode-map "M-s j"
                        'consult-org-heading)))

(provide 'keys)
;;; keys.el ends here
