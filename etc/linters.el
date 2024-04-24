;;; linters.el -- Customizations of Flymake and other linting stuff  ;; -*- lexical-binding: t; -*-

;;; Commentary:
;;;; Migrated from Flycheck -> Flymake, spring break 2023

;;; Code:
;;; FLYMAKE
(use-package flymake
  :ensure flymake
  :demand
  :defines
  flymake-mode
  :hook (prog-mode . flymake-mode)
  :bind
  (:map flymake-mode-map
        ("M-n"     . flymake-goto-next-error)
        ("M-p"     . flymake-goto-prev-error)
        ("C-c !"   . flymake-show-buffer-diagnostics)))



(use-package flymake-collection
  :ensure flymake-collection
  :after (flymake)
  :hook
  (after-init . flymake-collection-hook-setup)
  (markdown-mode . flymake-mode ))

(use-package flymake-css
  :ensure flymake-css
  :after (flymake)
  :hook (css-mode . flymake-css-load))

(use-package flymake-json
  :ensure flymake-json
  :after flymake
  :hook
  ((json-mode json-ts-mode) . flymake-json-load))


(use-package flymake-shellcheck
  :ensure nil ;; included in Emacs 29
  :after (flymake)
  :hook
  ((sh-mode sh-ts-mode) . flymake-shellcheck-load))

(use-package flymake-yamllint
  :ensure flymake-yamllint
  :after (flymake)
  :hook
  (yaml-mode . flymake-yamllint-setup))

(use-package sideline
  :ensure t)

(use-package sideline-flymake
  :defines
  sideline-flymake-display-mode
  sideline-backends-right
  :ensure t
  :hook (flymake-mode . sideline-mode)
  :init
  (setq sideline-flymake-display-mode 'point) ; 'point to show errors only on point
                                        ; 'line to show errors on the current line
  (setq sideline-backends-right '(sideline-flymake)))

(provide 'linters)
;;; linters.el ends here
