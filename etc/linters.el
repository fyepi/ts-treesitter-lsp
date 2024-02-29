;;; linters.el -- Customizations of Flymake and other linting stuff  ;; -*- lexical-binding: t; -*-

;;; Commentary:
;;;; Migrated from Flycheck -> Flymake, spring break 2023

;;; Code:
;;; FLYMAKE
(use-package flymake
  :ensure flymake
  :demand
  :hook (prog-mode . flymake-mode)
  :defines
  flymake-mode
  :bind
  ("M-n"     . flymake-goto-next-error)
  ("M-p"     . flymake-goto-prev-error)
  ("<f12>"   . flymake-goto-next-error)
  ("C-<f12>" . flymake-goto-next-error)
  ("C-c !"   . flymake-show-buffer-diagnostics))

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

(use-package flymake-cursor
  :ensure flymake-cursor
  :after (flymake)
  :demand t
  :custom
  (flymake-cursor-error-display-delay 0.1)
  (flymake-cursor-number-of-errors-to-display nil)
  (flymake-cursor-auto-enable t))

(use-package flymake-diagnostic-at-point
  :ensure flymake-diagnostic-at-point
  :after (flymake)
  :defines
  flymake-diagnostic-at-point-error-prefix
  :hook
  (flymake-mode . flymake-diagnostic-at-point-mode)
  :custom
  (flymake-diagnostic-at-point-timer-delay 0.1)
  (flymake-diagnostic-at-point-display-diagnostic-function
   #'os/display-flymake-diagnostic-in-popup-and-minibuffer))


(use-package flymake-eslint
  :ensure flymake-eslint
  :after flymake
  :hook
  (js2-mode . flymake-eslint-enable)
  (web-mode .  flymake-eslint-enable))


(defun os/enable-eslint-if-typescript ()
  "Enable eslint if typescript mode"
  (when (or (eq major-mode 'tsx-ts-mode)
            (eq major-mode 'typescript-ts-mode))
    (flymake-eslint-enable)))

;; Eglot overrides flymake backends so we need to wait until
;; eglot is initialised before we can enable the esling flymake backend
(add-hook 'eglot-managed-mode-hook #'os/enable-eslint-if-typescript)

(use-package flymake-json
  :ensure flymake-json
  :after flymake
  :hook
  (js-mode . flymake-json-load)
  (typescript-ts-mode . flymake-json-load)
  (tsx-ts-mode . flymake-json-load)
  (json-mode . flymake-json-load)
  (web-mode . flymake-json-load))

(use-package flymake-ruff
  :ensure flymake-ruff
  :after (flymake)
  :hook
  (python-base-mode . flymake-ruff-load))

(use-package flymake-sass
  :ensure flymake-sass
  :after (flymake)
  :hook
  (sass-mode . flymake-sass-load))

(use-package flymake-shellcheck
  :ensure nil ;; included in Emacs 29
  :after (flymake)
  :hook
  (sh-mode . flymake-shellcheck-load))

(use-package flymake-yamllint
  :ensure flymake-yamllint
  :after (flymake)
  :hook
  (yaml-mode . flymake-yamllint-setup))

(declare-function popup-tip "popup")
(defun os/display-flymake-diagnostic-in-popup-and-minibuffer (text)
  "Display flymake diagonstic TEXT in minibuffer and popup."
  (popup-tip (concat flymake-diagnostic-at-point-error-prefix text))
  (message (concat flymake-diagnostic-at-point-error-prefix text)))

(provide 'linters)
;;; linters.el ends here
