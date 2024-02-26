;; completions.el --- auto-complete related config  ;; -*- lexical-binding: t; -*-

;;; Commentary:
;;;; Packages and functions related to completion, both in buffers and
;;;; the minibuffer

;;; Code:
;;; CONSULT
(use-package consult
  :ensure consult
  :hook
  (completion-list-mode . consult-preview-at-point-mode))


;;; CORFU
(use-package corfu
  :ensure corfu
  :commands
  corfu-complete
  corfu-popupinfo-mode
  corfu-quick-complete
  global-corfu-mode
  :defines
  corfu-map
  corfu-margin-formatters
  :bind
  (:map corfu-map
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)
        ("<escape>" . corfu-quit)
        ("<return>" . corfu-insert)
        ("M-a" . corfu-popupinfo-beginning)
        ("M-d" . corfu-popupinfo-documentation)
        ("M-e" . corfu-popupinfo-end)
        ("M-l" . corfu-info-location)
        ("M-n" . corfu-popupinfo-scroll-up)
        ("M-p" . corfu-popupinfo-scroll-down)
        ("SPC" . corfu-insert-separator))
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)       ; Always have the same width
  (corfu-count 10)
  (corfu-scroll-margin 4)
  (corfu-popupinfo-delay 0.2)
  (corfu-quit-at-boundary 'separator)
  (corfu-separtor ?\s) ;; space
  (corfu-quit-no-match 'separator)
  (confu-preview-current 'insert)
  :config
  (global-corfu-mode)
  (corfu-popupinfo-mode))


;;; EGLOT
(use-package eglot
  :ensure eglot
  :defines
  eglot-server-programs
  :functions
  eglot-signature-eldoc-function
  :hook
  ;; https://lists.gnu.org/archive/html/emacs-devel/2023-02/msg00841.html
  (eglot-managed-mode . eglot-inlay-hints-mode)
  (prog-mode . eglot-ensure)
  :bind
  (:map eglot-mode-map
        ("C-c c a" . eglot-code-actions)
        ("C-c c o" . eglot-code-actions-organize-imports)
        ("C-c c r" . eglot-rename)
        ("C-c c f" . eglot-format))
  :custom
  (eglot-autoshutdown t)
  :config
  ;; use typescript-languange-server instead of the eglot default --
  ;; provides better support for external deps
  ;;
  ;; note: have to use `equal' as the comparator function because of
  ;; the list-ness of the key in the alist
  (setf (alist-get '(js2-mode tsx-ts-mode typescript-ts-mode) eglot-server-programs "" nil 'equal)
        '("typescript-language-server" "--stdio"))

  ;; Make Pandas play well with Pyright
  ;;; https://www.reddit.com/r/emacs/comments/swqr6o/how_to_get_pyright_complete_pandas/
  (setq-default eglot-workspace-configuration
                '((:pyright . ((useLibraryCodeForTypes . t)))))

  ;; from
  ;; https://www.reddit.com/r/emacs/comments/1447fy2/looking_for_help_in_improving_typescript_eglot/
  ;; -- speed up?
  (declare-function jsonrpc--log-event "jsonrpc")
  (fset #'jsonrpc--log-event #'ignore)

  ;; add inlay hints to typescript
  ;;; https://www.reddit.com/r/emacs/comments/11bqzvk/emacs29_and_eglot_inlay_hints/
  (add-to-list
   'eglot-server-programs
   '((js2-mode tsx-ts-mode typescript-ts-mode)
     "typescript-language-server" "--stdio"
     :initializationOptions
     (:preferences
      (
       :includeInlayParameterNameHints "all"
       :includeInlayParameterNameHintsWhenArgumentMatchesName t
       :includeInlayFunctionParameterTypeHints t
       :includeInlayVariableTypeHints t
       :includeInlayVariableTypeHintsWhenTypeMatchesName t
       :includeInlayPRopertyDeclarationTypeHints t
       :includeInlayFunctionLikeReturnTypeHints t
       :includeInlayEnumMemberValueHints t)))))

(declare-function eglot-signature-eldoc-function "eglot")
(use-package eglot-signature-eldoc-talkative
  :ensure eglot-signature-eldoc-talkative
  :functions
  eglot-signature-eldoc-talkative
  :config
  (advice-add #'eglot-signature-eldoc-function
              :override #'eglot-signature-eldoc-talkative))

;;; HIPPY-EXPAND
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))


;;; KIND-ICON
(use-package kind-icon
  :ensure kind-icon
  :after (corfu)
  :defines
  corfu-margin-formatters ;; this is a lie
  :functions
  kind-icon-margin-formatter
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)        ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


;;; MARGINALIA
(use-package marginalia
  :ensure marginalia
  :functions
  marginalia-mode
  :defines
  marginalia-align
  marginalia-command-categories
  :bind
  (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode)
  :config
  (add-to-list 'marginalia-command-categories '(os/find-file . file))
  (add-to-list 'marginalia-command-categories '(projectile-find-file . file)))

;;; ORDERLESS
(use-package orderless
  :ensure orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))
                                   (eglot (styles basic partial-completion))))
  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp
     orderless-flex)))     ;; Basically fuzzy finding


;;; SMART-TAB
(use-package smart-tab
  :ensure smart-tab
  :diminish
  :functions
  global-smart-tab-mode
  :custom
  (smart-tab-completion-functions-alist '((text-mode . dabbrev-completion)))
  (smart-tab-expand-eolp t)
  (smart-tab-user-provided-completion-function 'completion-at-point)
  (smart-tab-using-hippie-expand t)
  :config
  (global-smart-tab-mode 1))


;;; VERTICO
(use-package vertico
  :ensure vertico
  :defines
  vertico-map
  :functions
  vertico-mode
  :bind
  (:map vertico-map
        ("<escape>" . minibuffer-keyboard-quit)
        ("<tab>"    . vertico-insert))
  :custom
  (vertico-count 20)  ; Number of candidates to display
  (vertico-cycle t) ; Go from last to first candidate and first to last (cycle)?
  (vertico-resize t)
  :init
  (vertico-mode))

(use-package consult-dir
  :ensure consult-dir)

(use-package vertico-directory
  :after vertico
  :ensure vertico-directory
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))

  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package embark
  :ensure embark)

(use-package embark-consult
  :ensure embark-consult
  :hook
  (embark-collection-mode .consult-preview-at-point-mode))

(provide 'completions)
;;; completion.el ends here
