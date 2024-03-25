;; themes.el --- theme stuff  ;; -*- lexical-binding: t; -*-

;;; Commentary:
;;;; Theme-related customization

;;; Code:
(use-package solarized-theme
  :ensure solarized-theme)

(use-package doom-themes
  :ensure doom-themes)

(defun os/solarize-this ()
  "Enable solarized-dark theme."
  (interactive)
  (load-theme 'solarized-dark t))

(defun os/solarize-this-light ()
  "Enable solarized-light theme."
  (interactive)
  (load-theme 'solarized-light t))

(load-theme 'doom-palenight)

(provide 'themes)
;;; themes.el ends here
