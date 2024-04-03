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
  (corfu-auto-delay 0.0)
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
  :preface
  (defun eglot-disable-in-cider ()
    (when (eglot-managed-p)
      (if (bound-and-true-p cider-mode)
          (progn
            (remove-hook 'completion-at-point-functions 'eglot-completion-at-point t)
            (remove-hook 'xref-backend-functions 'eglot-xref-backend t))
        (add-hook 'completion-at-point-functions 'eglot-completion-at-point nil t)
        (add-hook 'xref-backend-functions 'eglot-xref-backend nil t))))
  :defines
  eglot-server-programs
  :functions
  eglot-signature-eldoc-function
  :hook (((tsx-mode typescript-ts-mode
                    prisma-ts-mode js2-mode python-ts-mode) . eglot-ensure)

         ((cider-mode eglot-managed-mode) . eglot-disable-in-cider))
  ;; https://lists.gnu.org/archive/html/emacs-devel/2023-02/msg00841.html
  ;; (eglot-managed-mode . eglot-inlay-hints-mode))
  :bind
  (:map eglot-mode-map
        ("C-c c a" . eglot-code-actions)
        ("C-c c o" . eglot-code-actions-organize-imports)
        ("C-c c r" . eglot-rename)
        ("C-c c f" . eglot-format))
  :custom
  (eglot-connect-timeout 100)
  (eglot-autoshutdown t)
  (eglot-sync-connect nil)
  (eglot-ignored-server-capabilities
   '(:documentFormattingProvider
     :documentRangeFormattingProvider
     :documentOnTypeFormattingProvider
     :colorProvider
     :foldingRangeProvider))
  :config
  ;; use typescript-languange-server instead of the eglot default --
  ;; provides better support for external deps
  ;;
  ;; note: have to use `equal' as the comparator function because of
  ;; the list-ness of the key in the alist
  (setf (alist-get '(js2-mode typescript-ts-mode) eglot-server-programs "" nil 'equal)
        '("typescript-language-server" "--stdio"))


  (setf (alist-get '(prisma-ts-mode) eglot-server-programs "" nil 'equal)
        '("prisma-language-server" "--stdio"))

  (setf (alist-get '(clojure-mode clojurec-mode clojurescript-mode) eglot-server-programs "" nil 'equal)
        '("clojure-lsp"))

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
   '((js2-mode tsx-mode tsx-ts-mode typescript-ts-mode)
     "typescript-language-server" "--stdio"
     :initializationOptions
     (:preferences
      (
       :includeInlayParameterNameHints "all"
       :includeInlayParameterNameHintsWhenArgumentMatchesName t
       :includeInlayFunctionParameterTypeHints t
       :includeInlayVariableTypeHints t
       :includeInlayVariableTypeHintsWhenTypeMatchesName t
       :includeInlayPropertyDeclarationTypeHints t
       :includeInlayFunctionLikeReturnTypeHints t
       :includeInlayEnumMemberValueHints t)))))

(declare-function eglot-signature-eldoc-function "eglot")

(defun os/xref-use-cider ()
  "Use CIDER as the completion function for xref."
  (interactive)
  (setq-local xref-backend-functions '(cider--xref-backend)))


(defun os/xref-use-eglot ()
  "Use CIDER as the completion function for xref."
  (interactive)
  (setq-local xref-backend-functions '(eglot-xref-backend)))

(use-package eglot-signature-eldoc-talkative
  :ensure eglot-signature-eldoc-talkative
  :functions
  eglot-signature-eldoc-talkative
  :config
  (advice-add #'eglot-signature-eldoc-function
              :override #'eglot-signature-eldoc-talkative))

(use-package jarchive
  :ensure t
  :defines
  jarchive-setup
  :after eglot
  :config
  (jarchive-setup))

;; LSP & companions used for clojure + Java as experience is better
;; For rest, use eglot
(use-package lsp-mode
  :ensure t
  :hook ((lsp-mode . lsp-diagnostics-mode))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-diagnostics-provider :flymake)
  ;; (lsp-completion-provider :none)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil)
  (lsp-keep-workspace-alive nil)
  (lsp-idle-delay 0.5)
  (lsp-enable-xref t)
  (lsp-signature-doc-lines 1))

(use-package lsp-completion
  :no-require
  :hook ((lsp-mode . lsp-completion-mode-maybe))
  :commands (lsp-completion-mode)
  :preface
  (defun lsp-completion-mode-maybe ()
    (unless (bound-and-true-p cider-mode)
      (lsp-completion-mode 1))))

(use-package lsp-treemacs
  :ensure t
  :defer t
  :custom
  (lsp-treemacs-theme "Iconless"))

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
         . lsp))

(use-package lsp-java
  :ensure t
  :after lsp-mode
  :hook (java-mode . lsp))

(use-package lsp-metals
  :ensure t
  :after lsp-mode
  :hook (scala-mode . lsp)
  :custom
  (lsp-metals-server-args
   '("-J-Dmetals.allow-multiline-string-formatting=off")))

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

(provide 'completions)
;;; completions.el ends here
