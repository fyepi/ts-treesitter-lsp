;; completions.el --- auto-complete related config  ;; -*- lexical-binding: t; -*-

;;; Commentary:
;;;; Packages and functions related to completion, both in buffers and
;;;; the minibuffer

;;; Code:
;;; CONSULT
(use-package consult
  :defines
  consult-customize
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )



;;; CORFU

(use-package corfu
  :ensure t
  :bind ( :map corfu-map
          ("TAB" . corfu-next)
          ([tab] . corfu-next)
          ("S-TAB" . corfu-previous)
          ([backtab] . corfu-previous)
          ([remap completion-at-point] . corfu-complete)
          ("RET" . corfu-complete-and-quit)
          ("<return>" . corfu-complete-and-quit))
  :commands (corfu-quit)
  :custom
  (corfu-cycle t)
  (corfu-preselect-first t)
  (corfu-scroll-margin 4)
  (corfu-quit-no-match t)
  (corfu-quit-at-boundary t)
  (corfu-max-width 100)
  (corfu-min-width 42)
  (corfu-count 9)
  ;; should be configured in the `indent' package, but `indent.el'
  ;; doesn't provide the `indent' feature.
  (tab-always-indent 'complete)
  :config
  (defun corfu-complete-and-quit ()
    (interactive)
    (corfu-complete)
    (corfu-quit))
  :hook (after-init . global-corfu-mode))

(use-package corfu-popupinfo
  :bind ( :map corfu-popupinfo-map
          ("M-p" . corfu-popupinfo-scroll-down)
          ("M-n" . corfu-popupinfo-scroll-up))
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom-face
  (corfu-popupinfo ((t :height 1.0))))


(use-package cape
  :ensure t
  :after corfu
  :config
  (setq completion-at-point-functions '(cape-file)))

;; LSP
(use-package lsp-mode
  :ensure t
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         ((tsx-ts-mode typescript-ts-mode tsx-mode) . lsp-deferred))
  :custom
  (lsp-keymap-prefix "C-c c")
  (lsp-completion-provider :none) ;; we use Corfu
  (lsp-diagnostics-provider :flycheck)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil)
  (lsp-keep-workspace-alive nil)
  (lsp-idle-delay 0.5)
  ;; core
  (lsp-enable-xref t)
  (lsp-auto-configure t)
  (lsp-eldoc-enable-hover t)
  (lsp-enable-dap-auto-configure t)
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (lsp-enable-imenu t)
  (lsp-enable-indentation nil)
  (lsp-enable-links t)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-suggest-server-download nil)
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-text-document-color nil)
  ;; completion
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t)
  (lsp-enable-snippet t)
  (lsp-completion-show-kind t)
  ;; headerline
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  ;; modeline
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-modeline-workspace-status-enable nil)
  (lsp-signature-doc-lines 1)
  ;; lens
  (lsp-lens-enable t)
  ;; semantic
  (lsp-semantic-tokens-enable nil)
  :init
  (setq lsp-use-plists t))


(use-package lsp-completion
  :no-require
  :hook ((lsp-mode . lsp-completion-mode-maybe))
  :commands (lsp-completion-mode)
  :preface
  (defun lsp-completion-mode-maybe ()
    (unless (bound-and-true-p cider-mode)
      (lsp-completion-mode 1))))


(use-package lsp-clojure
  :demand t
  :after lsp-mode
  :hook (cider-mode . cider-toggle-lsp-completion-maybe)
  :preface
  (defun cider-toggle-lsp-completion-maybe ()
    (lsp-completion-mode (if (bound-and-true-p cider-mode) -1 1))))

(use-package lsp-clojure
  :no-require
  :hook ((clojure-mode
          clojurec-mode
          clojurescript-mode)
         . lsp-deferred))

(use-package lsp-java
  :ensure t
  :demand t
  :after lsp-mode)

(use-package lsp-java
  :no-require
  :hook (java-mode . lsp-deferred))


(use-package lsp-metals
  :ensure t
  :after lsp-mode
  :hook (scala-mode . lsp)
  :custom
  (lsp-metals-server-args
   '("-J-Dmetals.allow-multiline-string-formatting=off")))

(use-package lsp-tailwindcss
  :straight '(lsp-tailwindcss :type git :host github :repo "merrickluo/lsp-tailwindcss")
  :init (setq lsp-tailwindcss-add-on-mode t)
  :config
  (dolist (tw-major-mode
           '(css-mode
             css-ts-mode
             typescript-mode
             typescript-ts-mode
             tsx-mode
             tsx-ts-mode
             js2-mode
             js-ts-mode))
    (add-to-list 'lsp-tailwindcss-major-modes tw-major-mode)))

(use-package lsp-ui
  :ensure t
  :after lsp-mode)

(use-package lsp-eslint
  :demand t
  :after lsp-mode)


(use-package jarchive
  :ensure t
  :defines
  jarchive-setup
  :after eglot
  :hook
  ((clojure-mode java-ts-mode java-mode) jarchive-setup))

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

(use-package embark
  :ensure embark)

(use-package embark-consult
  :ensure embark-consult
  :hook
  (embark-collection-mode .consult-preview-at-point-mode))


(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t
  :config (setq copilot-indent-offset-warning-disable t)
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("C-<tab>" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion)
              ("S-C-TAB" . 'copilot-accept-completion-by-word)
              ("S-C-<tab>" . 'copilot-accept-completion-by-word)))

(provide 'completions)
;;; completions.el ends here
