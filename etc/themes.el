;; themes.el --- theme stuff  ;; -*- lexical-binding: t; -*-

;;; Commentary:
;;;; Theme-related customization

;;; Code:

;;; For packaged versions which must use `require'.
(use-package modus-themes
  :ensure t
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))


  ;; Load the theme of your choice.
  (load-theme 'modus-vivendi-tinted)

  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))


(use-package rainbow-mode
  :ensure t
  :hook (prog-mode . rainbow-mode))

(use-package doom-modeline
  :defines
  doom-modeline-time
  doom-modeline-buffer-file-name-style
  :functions
  doom-modeline-mode
  :ensure t
  :init (doom-modeline-mode t)
  :config
  (setq doom-modeline-time t
        doom-modeline-buffer-file-name-style 'auto))

(provide 'themes)
;;; themes.el ends here
