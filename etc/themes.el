;; themes.el --- theme stuff  ;; -*- lexical-binding: t; -*-

;;; Commentary:
;;;; Theme-related customization

;;; Code:
(use-package doom-themes
  :defines
  doom-themes-enable-bold
  doom-themes-enable-italic
  doom-themes-treemacs-theme
  :functions
  doom-themes-org-config
  doom-themes-neotree-config
  doom-themes-visual-bell-config
  doom-themes-treemacs-config
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  (load-theme 'doom-tomorrow-day t))


(use-package doom-modeline
  :defines
  doom-modeline-time
  doom-modeline-buffer-file-name-style
  :functions
  doom-modeline-mode
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-time t
        doom-modeline-buffer-file-name-style 'auto))

(provide 'themes)
;;; themes.el ends here
