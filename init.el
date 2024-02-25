;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

(setq backup-directory-alist '((".*" . "~/.local/share/Trash/files")))

(use-package beacon)

(use-package doom-themes)
(use-package doom-modeline
  :init (doom-modeline-mode t))

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(use-package org-modern)
(use-package org-vcard
  :straight t)
;(with-eval-after-load 'org (global-org-modern-mode))

(use-package org-tempo
  :straight nil)

(use-package focus)

(use-package magit)

(use-package rainbow-mode
  :hook org-mode prog-mode)

(use-package rainbow-delimiters
  :hook ((org-mode . rainbow-delimiters-mode)
	 (prog-mode . rainbow-delimiters-mode)))

(use-package hl-todo
  :hook ((org-mode . hl-todo-mode)
         (prog-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

(use-package catppuccin-theme)

(use-package ef-themes)

(use-package which-key
  :straight t
  :init 
  (which-key-mode 1)
  :config
  (setq which-key-side-window-location 'bottom
	which-key-sort-order #'which-key-key-order
	which-key-allow-imprecise-window-fit nil
	which-key-sort-uppercase-first nil
	which-key-add-column-padding 1
	which-key-max-display-columns nil
	which-key-min-display-lines 6
	which-key-side-window-slot -10
	which-key-side-window-max-height 0.25
	which-key-idle-delay 0.8
	which-key-max-description-length 25
	which-key-allow-imprecise-window-fit nil
	which-key-separator " → " ))

(use-package vterm
  :config
  (setq shell-file-name "/bin/zsh"
	vterm-max-scrollback 5000))

(use-package vterm-toggle
  :after vterm
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 ;;(display-buffer-reuse-window display-buffer-in-direction)
                 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                 ;;(direction . bottom)
                 ;;(dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3))))
(global-set-key (kbd "M-n") 'vterm-toggle)
(add-hook 'vterm-mode
	  (lambda ()
	    (local-set-key (kbd "M-n") 'vterm -toggle)))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(add-hook 'dired-mode
	  (lambda ()
	    (local-set-key [_] 'dired-up-directory)))

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

;;install vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t))

(use-package vertico-directory
  :after vertico
  :straight nil
  :load-path "straight/repos/vertico/extensions/"
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))

  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completionustst-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be actived in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package embark
  :straight t)

(use-package embark-consult
  :straight t
  :hook
  (embark-collection-mode . consult-preview-at-point-mode))

(use-package projectile)

;;install dashboard
(use-package dashboard
  :straight t
  :init
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "emacs rocks")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content nil)
  (setq dashboard-items '((recents . 5)
                          (projects . 3)
                          (agenda . 3))))

:config
(dashboard-setup-startup-hook)

(use-package consult)



(use-package consult-dir
  :straight t
  :bind (("C-x C-d" . consult-dir)
	 :map vertico-map
	 ("C-x C-d" . consult-dir)
	 ("C-x C-j" . consult.dir-jump-file)))

(use-package perspective
  :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
  :init
  (persp-mode))

;;; CORFU
(use-package corfu
  :straight corfu
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

;; Using garbage magic hack.
(use-package gcmh
  :config
  (gcmh-mode 1))
;; Setting garbage collection threshold
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Silence compiler warnings as they can be pretty disruptive (setq comp-async-report-warnings-errors nil)

;; Silence compiler warnings as they can be pretty disruptive
(if (boundp 'comp-deferred-compilation)
    (setq comp-deferred-compilation nil)
  (setq native-comp-deferred-compilation nil))
;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

(defun os/org-path (path)
  (expand-file-name path org-directory))


(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-goto-interface "outline-path-completion")

(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
)

(setq org-directory "~/org/"
      org-agenda-files '("~/org/todo.org")
      org-default-notes-file (expand-file-name "notes.org" org-directory)
      org-ellipsis " ▼ "
      org-log-done 'time
      org-journal-dir "~/org/journal/"
      org-journal-date-format "%B %d, %Y (%A) "
      org-journal-file-format "%Y-%m-%d.org"
      org-hide-emphasis-markers t)
(setq org-src-preserve-indentation nil
      org-src-tab-acts-natively t
      org-edit-src-content-indentation 0
      org-src-fontify-natively t
      org-confirm-babel-evaluate nil)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "READ(r)" "|"  "DONE(d!)")
        (sequence "|" "WAIT(w)" "BACK(b)")))

(setq org-todo-keyword-faces
      '(("NEXT" . (:foreground "orange red" :weight bold))
        ("WAIT" . (:foreground "HotPink2" :weight bold))
        ("BACK" . (:foreground "MediumPurple3" :weight bold))))

;; Configure common tags
(setq org-tag-alist
      '((:startgroup)
                                        ; Put mutually exclusive tags here
        (:endgroup)
        ("@home" . ?H)
        ("@work" . ?W)
        ("batch" . ?b)
        ("followup" . ?f)))

(setq org-agenda-window-setup 'current-window)

;; Make done tasks show up in the agenda log
(setq org-log-done 'time)
(setq org-columns-default-format "%20CATEGORY(Category) %65ITEM(Task) %TODO %6Effort(Estim){:}  %6CLOCKSUM(Clock) %TAGS")
(setq org-agenda-custom-commands
      `(("d" "Dashboard"
         ((agenda "" ((org-deadline-warning-days 7)))
          (tags-todo "+PRIORITY=\"A\""
                     ((org-agenda-overriding-header "High Priority")))
          (tags-todo "+followup" ((org-agenda-overriding-header "Needs Follow Up")))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next Actions")
                 (org-agenda-max-todos nil)))
          (todo "WAIT"
                ((org-agenda-overriding-header "Waiting for")
                 (org-agenda-text-search-extra-files nil)))
          (todo "TODO"
                ((org-agenda-overriding-header "Unprocessed Inbox Tasks")
                 (org-agenda-text-search-extra-files nil)))))

        ("n" "Next Tasks"
         ((agenda "" ((org-deadline-warning-days 7)))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next Tasks")))))

        ;; Low-effort next actions
        ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
         ((org-agenda-overriding-header "Low Effort Tasks")
          (org-agenda-max-todos 20)
          (org-agenda-files org-agenda-files)))))
(setq org-capture-templates
      `(("t" "Todo" entry (file+headline "~/org/todo.org" "Inbox")
         "* TODO %?\n  %U\n  %a\n  %i" :prepend t :empty-lines 1)

        ("j" "Journal Entries")
        ("je" "General Entry" entry
         (file+olp+datetree ,(os/org-path "Journal.org"))
         "\n* %<%I:%M %p> - %^{Title} \n\n%?\n\n"
         :tree-type week
         :clock-in :clock-resume
	 :prepend t
         :empty-lines 1)
        ("jt" "Task Entry" entry
         (file+olp+datetree ,(os/org-path "Journal.org"))
         "\n* %<%I:%M %p> - Task Notes: %a\n\n%?\n\n"
         :tree-type week
         :clock-in :clock-resume
         :empty-lines 1)
        ("jj" "Journal" entry
         (file+olp+datetree ,(os/org-path "Journal.org"))
         "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
         :tree-type week
         :clock-in :clock-resume
         :empty-lines 1)))    

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; load dashboard instead of scratchpad at startup *INSTALL DASHBOARD*
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(load-theme 'catppuccin :no-confirm) ; Set theme
(menu-bar-mode -1) ; Turn menubar off
(scroll-bar-mode -1) ; Turn scrollbar off
(tool-bar-mode -1) ; Turn tool-bar off
(global-display-line-numbers-mode 1) ; Display line numbers globally
(beacon-mode 1) ; Turn beacon on
(setq inhibit-startup-screen t) ; Disable startup default startup screen
(setq auto-save-interval 1000)
(setq backup-directory-alist            '((".*" . "~/.Trash")))
(setq auto-save-list-file-prefix "~/.emacs.d/autosave/")

(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/autosave/" t)))


(use-package centered-window :straight t)

(electric-indent-mode -1)
(setq org-edit-src-content-indentation 0)

(set-face-attribute 'default nil 
		    :font "JetBrainsMono Nerd Font"
		    :height 130
		    :weight 'regular)
(set-face-attribute 'fixed-pitch nil
		    :font "JetBrainsMono Nerd Font"
		    :height 130
		    :weight 'regular)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
		    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
		    :slant 'italic)

(add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font-14"))

(setq pixel-scroll-precision-mode 1)
;; Uncomment the following line if line spacing needs adjusting.
;;(setq-default line-spacing 0.12)

(add-to-list 'default-frame-alist '(alpha-background . 95))
;; Needed if using emacsclient. Otherwise, your fonts will be smaller than expected.
;;(add-to-list 'default-frame-alist '(font . "SauceCodePro Nerd Font-16" :weight 'semibold))
;; changes certain keywords to symbols, such as lamda!
(setq global-prettify-symbols-mode t)
;;(add-hook 'find-file-hook (lambda () (set-face-attribute 'default nil :height 105)))

(global-set-key (kbd "C-+") 'text-scale-increase)
;;(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(setq next-line-add-newlines t)
(global-set-key (kbd "M-o") 'other-window)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-compression-mode nil)
 '(auto-encryption-mode nil)
 '(backup-directory-alist (list (cons ".*" (expand-file-name "~/.emacs.d/backup/"))))
 '(blink-cursor-mode nil)
 '(consult-buffer-sources '(consult--source-hidden-buffer consult--source-buffer))
 '(coverlay:mark-tested-lines nil)
 '(create-lockfiles nil)
 '(dirtrack-mode nil)
 '(display-line-numbers-grow-only t)
 '(eldoc-documentation-strategy 'eldoc-documentation-compose)
 '(fill-column 80)
 '(flymake-error-bitmap '(flymake-big-indicator compilation-error))
 '(flymake-eslint-defer-binary-check t)
 '(flymake-mode-line-format '(" ✔" flymake-mode-line-counters))
 '(flymake-note-bitmap '(flymake-big-indicator compilation-info))
 '(flymake-warning-bitmap '(flymake-big-indicator compilation-warning))
 '(fringe-mode '(24 . 0) nil (fringe))
 '(inhibit-startup-echo-area-message (user-login-name))
 '(inhibit-startup-screen t)
 '(kind-icon-blend-background nil)
 '(kind-icon-default-face 'corfu-default)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(2 ((shift) . hscroll) ((meta)) ((control) . text-scale)))
 '(projectile-show-menu nil)
 '(recentf-menu-path nil)
 '(recentf-menu-title nil)
 '(recentf-mode t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 101)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(use-dialog-box nil))

(defun os/prog-mode-hook ()
  "Internal function.  Configure some things common to all programming modes."
  (display-line-numbers-mode t)
  (show-paren-mode t)
  (subword-mode t)
  (display-fill-column-indicator-mode t)
  (local-unset-key (kbd "C-x C-p"))
  (local-set-key (kbd "C-x C-p") #'project-find-file)
  (flymake-mode t)
  ;; use a flycheck keybinding for flymake
  (local-set-key (kbd "C-c ! n") #'flymake-goto-next-error)
  ;; useful for when we switch source-control branches
  (add-hook
   'after-revert-hook
   'vc-refresh-state))

(add-hook
 'prog-mode-hook
 #'os/prog-mode-hook)

(winner-mode t)
(windmove-default-keybindings)

;;; EGLOT
(use-package eglot
  :straight eglot
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

  ;; from
  ;; https://www.reddit.com/r/emacs/comments/1447fy2/looking_for_help_in_improving_typescript_eglot/
  ;; -- speed up?
  (declare-function jsonrpc--log-event "jsonrpc")
  (fset #'jsonrpc--log-event #'ignore)

  ;; add inlay hints to typescript
  ;;; https://www.reddit.com/r/emacs/comments/11bqzvk/emacs29_and_eglot_inlay_hints/
  (add-to-list
   'eglot-server-programs
   '((js2-mode tsx-ts-mode tsx-mode typescript-ts-mode)
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

;;; Clojure
;;;

(defun clojure-coding-defaults ()
  "Use Lisp defaulst + extra clojure."
  ;; (eldoc-mode 1) ;; clojure-mode is calling this without positive arg to enable it
  (put-clojure-indent '$ 0)
  (setq fill-column 89
        clojure-docstring-fill-column 89))

;; key bindings and code colorization for Clojure
;; https://github.com/clojure-emacs/clojure-mode
(use-package clojure-mode
  :straight t
  :init
  ;; This is useful for working with camel-case tokens, like names of
  (add-hook 'clojure-mode-hook 'clojure-coding-defaults))


;; extra syntax highlighting for clojure
(use-package clojure-mode-extra-font-locking
  :straight t)

;;;;
;; Cider
;;;;

;; integration with a Clojure REPL
;; https://github.com/clojure-emacs/cider

(use-package cider
  :straight t)

;; eldoc conflicts with cider?
;; https://github.com/practicalli/spacemacs-content/issues/287
;; load before cider?
;; https://github.com/emacs-lsp/lsp-mode/issues/2445#issuecomment-751481500

;;(setq cider-jdk-src-paths '("/usr/lib/jvm/java-11-openjdk/lib/src.zip"))

(setq nrepl-popup-stacktraces nil)

(defun os/cider-mode-hook ()
  "Disable LSP completion when cider is started."
  (eldoc-mode 1))

(defun os/cider-repl-mode-hook ()
  (eldoc-mode 1))

(add-hook 'cider-mode-hook 'os/cider-mode-hook)
(add-hook 'cider-repl-mode-hook 'os/cider-repl-mode-hook)

(require 'eglot)
(require 'flymake)
(require 'treesit)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; this fixes a problem where v0.20.4 of this grammar blows up with emacs
(defvar os/tsx-treesit-auto-recipe
  (make-treesit-auto-recipe
   :lang 'tsx
   :ts-mode 'tsx-ts-mode
   :remap '(typescript-tsx-mode)
   :requires 'typescript
   :url "https://github.com/tree-sitter/tree-sitter-typescript"
   :revision "v0.20.3"
   :source-dir "tsx/src"
   :ext "\\.tsx\\'")
  "Recipe for libtree-sitter-tsx.dylib")
(add-to-list 'treesit-auto-recipe-list os/tsx-treesit-auto-recipe)

(defvar os/typescript-treesit-auto-recipe
  (make-treesit-auto-recipe
   :lang 'typescript
   :ts-mode 'typescript-ts-mode
   :remap 'typescript-mode
   :requires 'tsx
   :url "https://github.com/tree-sitter/tree-sitter-typescript"
   :revision "v0.20.3"
   :source-dir "typescript/src"
   :ext "\\.ts\\'")
  "Recipe for libtree-sitter-typescript.dylib")
(add-to-list 'treesit-auto-recipe-list os/typescript-treesit-auto-recipe)

(use-package
  flymake-eslint
  :straight t)


;; format on save
(use-package
  apheleia
  :delight
  :straight t
  :config
  (add-to-list
   'apheleia-mode-alist
   '(tsx-ts-mode . prettier-typescript)))

;; code-coverage overlays
(use-package
  coverlay
  :delight coverlay-minor-mode
  :straight t)

;; CSS-in-JS support for tsx-mode
(use-package
  css-in-js-mode
  :delight
  :straight
  '(css-in-js-mode :type git :host github :repo "orzechowskid/tree-sitter-css-in-js" :branch "main" :post-build ((require 'css-in-js-mode) (css-in-js-mode-fetch-shared-library))))

;; linter adapter which doesn't use LSP
(use-package
  flymake-eslint
  :straight t)

;; code-folding
;; origami depends on some now-deprecated cl functions and there's not much we
;; can do about that
(let ((byte-compile-warnings '((not cl-functions))))
  (use-package
    origami
    :delight
    :straight t))

;; major-mode for JS/TS/JSX/TSX
(use-package
  tsx-mode
  :straight '(tsx-mode :type git :host github :repo "orzechowskid/tsx-mode.el" :branch "emacs29")
  :mode (("\\.[jt]s[x]?\\'" . tsx-mode)
         ("\\.[mc]js\\'" . tsx-mode)))

(use-package
  nov
  :straight t
  :config (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))
