;;; linters.el -- Customizations of Flymake and other linting stuff  ;; -*- lexical-binding: t; -*-

;;; Commentary:
;;; Currently just flycheck

;;; Code:
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error)))

(provide 'linters)
;;; linters.el ends here
