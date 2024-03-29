;; misc-packages.el --- various customizations and additions of packages  ;; -*- lexical-binding: t; -*-

;;; Commentary:
;;;; This is for packages that _aren't_ built-in to Emacs but aren't
;;;; complex enough to justify a distinct file

;;; Code:
;;; PROJECTILE
;;;; this is at the top because things below require it.
(use-package projectile
  :ensure projectile
  :diminish
  :defines
  projectile-mode-map
  :functions
  projectile-cleanup-known-projects
  projectile-find-file
  projectile-mode
  projectile-parent
  projectile-project-p
  projectile-project-root
  :hook
  (projectile-after-switch-project . os/node-project-setup)
  :bind
  ("C-c a"   . projectile-ag)
  ("C-c C-o" . projectile-multi-occur)
  (:map projectile-mode-map ("C-c p" . projectile-command-map))
  :bind*
  ("C-c C-a" . projectile-ag) ;; fuck you js2-mode
  :init
  (projectile-mode +1)
  :custom
  (projectile-cache-file
   (expand-file-name ".projectile.cache" os/emacs-tmp-dir))
  (projectile-globally-ignored-files '("TAGS" ".git" ".DS_Store"))
  (projectile-known-projects-file
   (expand-file-name "projectile-bookmarks.eld" os/emacs-tmp-dir))
  (projectile-switch-project-action 'projectile-dired)
  :config
  (projectile-cleanup-known-projects))


(use-package treemacs
  :ensure t
  :defer t
  :defines
  treemacs-collapse-dirs
  treemacs-deferred-git-apply-delay
  treemacs-directory-name-transformer
  treemacs-display-in-side-window
  treemacs-eldoc-display
  treemacs-file-event-delay
  treemacs-file-extension-regex
  treemacs-file-follow-delay
  treemacs-file-name-transformer
  treemacs-follow-after-init
  treemacs-expand-after-init
  treemacs-find-workspace-method
  treemacs-git-command-pipe
  treemacs-goto-tag-strategy
  treemacs-header-scroll-indicators
  treemacs-hide-dot-git-directory
  treemacs-indentation
  treemacs-indentation-string
  treemacs-is-never-other-window
  treemacs-max-git-entries
  treemacs-missing-project-action
  treemacs-move-forward-on-expand
  treemacs-no-png-images
  treemacs-no-delete-other-windows
  treemacs-project-follow-cleanup
  treemacs-persist-file
  treemacs-position
  treemacs-read-string-input
  treemacs-recenter-distance
  treemacs-recenter-after-file-follow
  treemacs-recenter-after-tag-follow
  treemacs-recenter-after-project-jump
  treemacs-recenter-after-project-expand
  treemacs-litter-directories
  treemacs-project-follow-into-home
  treemacs-show-cursor
  treemacs-show-hidden-files
  treemacs-silent-filewatch
  treemacs-silent-refresh
  treemacs-sorting
  treemacs-select-when-already-in-treemacs
  treemacs-space-between-root-nodes
  treemacs-tag-follow-cleanup
  treemacs-tag-follow-delay
  treemacs-text-scale
  treemacs-user-mode-line-format
  treemacs-user-header-line-format
  treemacs-wide-toggle-width
  treemacs-width
  treemacs-width-increment
  treemacs-width-is-initially-locked
  treemacs-workspace-switch-cleanup
  treemacs-last-period-regex-value
  treemacs-python-executable
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package whole-line-or-region
  :ensure whole-line-or-region)

(require 'whole-line-or-region)

;;; AG
(use-package ag
  :ensure ag
  :commands
  ag
  :bind
  ("C-c C-A" . ag-regexp-project-regexp)
  :custom
  (ag-highlight-search t)
  (ag-reuse-buffers t))


;;; ALLLLLLL THE ICONS
(use-package all-the-icons
  :ensure all-the-icons)

(use-package all-the-icons-completion
  :ensure all-the-icons-completion
  :after (marginalia all-the-icons)
  :functions
  all-the-icons-completion-mode
  :hook
  (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package all-the-icons-dired
  :ensure all-the-icons-dired
  :diminish)

(use-package all-the-icons-ibuffer
  :ensure all-the-icons-ibuffer
  :after (ibuffer)
  :functions
  all-the-icons-ibuffer-mode
  :config
  (all-the-icons-ibuffer-mode 1))


;;; APHELEIA
;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia
(use-package apheleia
  :ensure apheleia
  :diminish ""
  :defines
  apheleia-formatters
  apheleia-mode-alist
  :functions
  apheleia-global-mode
  :config
  (setf (alist-get 'shfmt apheleia-formatters)
        '("shfmt" "-i=4" "-sr" "-kp"))
  ;; https://git.genehack.net/os/emacs/issues/2
  (setf (alist-get 'prettier-json apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (setf (alist-get 'python-mode apheleia-mode-alist) 'ruff)
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff)
  (apheleia-global-mode +1))


;;; API BLUEPRINT
(use-package apib-mode
  :ensure apib-mode
  :mode
  "\\.apib\\'")


;;; BROWSE-KILL-RING
(use-package browse-kill-ring
  :ensure browse-kill-ring
  :commands
  browse-kill-ring
  browse-kill-ring-default-keybindings
  :init
  (browse-kill-ring-default-keybindings))


;;; DASHBOARD
(use-package dashboard
  :disabled t ;;; gives warnings about `Invalid face attribute
              ;;; :inherit 'warning' and `Invalid face reference:
              ;;; quote'
  :ensure dashboard
  :hook
  (dashboard-mode . (lambda () (setq show-trailing-whitespace nil)))
  :config
  (setq dashboard-banner-logo-title "welcome to john’s emacs! ")
  (if (display-graphic-p)
      (setq dashboard-startup-banner 'logo)
    (setq dashboard-startup-banner 2))
  (setq dashboard-center-content t)
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-navigator-buttons
        '(;; line1
          (("" "Scratch" "Scratch buffer"
            (lambda (&rest _) (os/create-scratch-buffer)) 'warning "[ " " ]")
           ("" "Projectile" "Projectile buffer"
            (lambda (&rest _) (projectile-switch-project)) 'default "[ " " ]")
           ("" "Home" "Home directory"
            (lambda (&rest _) (find-file "~")) 'all-the-icons-blue  "[ " " ]"))
          ))
  (setq dashboard-set-footer nil)
  (setq dashboard-items '((projects . 10)))
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (dashboard-setup-startup-hook))


;;; DIMINISH
;;;; from http://whattheemacsd.com/init.el-04.html
(use-package diminish
  :ensure diminish)


;;; DISK
(use-package disk
  :ensure disk
  :bind
  ("<f1>" . disk))


;;; EDITORCONFIG
(use-package editorconfig
  :ensure editorconfig
  :diminish
  :functions
  editorconfig-mode
  :config
  (editorconfig-mode 1))


;;; EMOJIFY
(use-package emojify
  :ensure emojify
  :hook (after-init . global-emojify-mode)
  :custom
  (emojify-display-style 'unicode)
  (emojify-emoji-styles '(unicode))
  (emojify-show-help nil)
  :config
  (when (member "Apple Color Emoji" (font-family-list))
    (message "Loading apple emojis")
    (set-fontset-font t 'symbol "Apple Color Emoji" nil 'prepend)
    (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)))

;;; EXPAND-REGION
(use-package expand-region
  :ensure expand-region
  :bind
  ("C-=" . er/expand-region))


;;; FANCY-COMPILATION MODE
(use-package fancy-compilation
  :ensure fancy-compilation
  :commands
  fancy-compilation-mode)
(with-eval-after-load 'compile (fancy-compilation-mode))


;;; FILLADAPT -- WTF isn't this part of emacs by default by now?!
(use-package filladapt
  :ensure filladapt
  :diminish "fa"
  :functions
  filladapt-mode
  :config
  (filladapt-mode 1))


;;; GIT COMMIT MODE
(use-package git-commit
  :ensure git-commit
  :commands
  git-commit)


;;; GIT-GUTTER
(use-package git-gutter
  :ensure git-gutter
  :diminish ""
  :functions
  global-git-gutter-mode
  :config
  (global-git-gutter-mode t))


;;; HANDLEBARS
(use-package handlebars-mode
  :ensure handlebars-mode
  :commands
  handlebars-mode)


;;; HELPFUL
(use-package helpful
  :ensure helpful
  :bind
  ("C-c h" . helpful-at-point))


;;; JSON
(use-package json-mode
  :ensure json-mode
  :after (add-node-modules-path)
  :commands
  json-mode
  json-ts-mode
  :hook
  (json-mode . add-node-moules-path)
  (json-ts-mode . add-node-moules-path)
  :flymake-hook
  (json-mode (flymake-collection-jsonlint))
  (json-ts-mode (flymake-collection-jsonlint))
  :mode
  "\\.json\\'"
  :config
  (add-to-list 'safe-local-variable-values '(json-mode-indent-level . 2))
  (add-to-list 'safe-local-variable-values '(json-mode-indent-level . 4))
  (setq-default json-mode-indent-level 2))

(use-package json-navigator
  :ensure json-navigator
  :after json-mode)

(use-package jq-format
  :ensure jq-format
  :after json-mode)

;;; JAVASCRIPT / NODE / TYPESCRIPT PACKAGES
(use-package add-node-modules-path
  :ensure add-node-modules-path
  :commands
  add-node-modules-path)

(use-package eslint-fix
  :ensure eslint-fix)

(use-package js2-mode
  :ensure js2-mode
  :diminish "js2"
  :commands
  js2-mode
  :defines
  js2-additional-externs
  js2-mode-map
  :hook
  ((js2-mode . add-node-modules-path)
   (js2-mode . (lambda ()
                 (if (string-match "\\/test\\/" (buffer-file-name))
                     (setq js2-additional-externs
                           `("describe" "it" "before" "after")))))
   (js2-mode .  (lambda() (add-hook 'after-save-hook 'eslint-fix nil t))))
  :bind
  (:map js2-mode-map ("C-?" . os/js2-insert-debug))
  :interpreter
  "node"
  :mode
  "\\.jsx?\\'"
  :custom
  (js2-global-externs '("Promise"))
  (js2-highlight-level 3)
  (js2-include-browser-externs nil)
  (js2-include-node-externs t)
  (js2-mode-assume-strict t)
  :config
  (add-to-list 'safe-local-variable-values '(js2-basic-offset . 2))
  (add-to-list 'safe-local-variable-values '(js2-basic-offset . 4))
  (setq-default js2-basic-offset 2))


;;; KOLON-MODE
(use-package kolon-mode
  :ensure kolon-mode
  :commands
  kolon-mode)

;;; LIGATURES
(defvar os/path-to-ligature-repo (expand-file-name "~/src/ligature.el")
  "Path to ligature repo.")
(use-package ligature
  :if (file-exists-p os/path-to-ligature-repo)
  :load-path os/path-to-ligature-repo
  :defines
  global-ligature-mode
  ligature-set-ligatures
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures
   'prog-mode
   '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
     ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
     "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
     "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
     "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
     "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
     "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
     "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
     ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
     "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
     "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
     "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
     "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))


;;; MAGIT
(use-package magit
  :ensure magit
  :if os/git-executable
  :commands
  magit-status
  :defines
  magit-status-mode-map
  :functions
  magit-get
  magit-get-all
  magit-get-current-branch
  magit-git-string
  magit-status-setup-buffer
  :bind
  (:map magit-status-mode-map ("q" . os/magit-quit-session)))


;;; MARKDOWN
(use-package markdown-mode
  :ensure markdown-mode
  :commands
  markdown-mode
  :defines
  markdown-mode-map
  :magic
  "\\.mr?kd"
  :bind
  (:map markdown-mode-map
        ("M-n" . flymake-goto-next-error)
        ("M-p" . flymake-goto-prev-error)))

(use-package html-to-markdown
  :ensure html-to-markdown
  :commands
  html-to-markdown-string)


;;; MOVE TEXT
(use-package move-text
  :ensure move-text
  :functions
  move-text-default-bindings
  :config
  (move-text-default-bindings))


;;; MULTI-TERM
(setq system-uses-terminfo nil)
(use-package multi-term
  :ensure multi-term
  :commands
  multi-term
  term
  :defines
  term-buffer-maximum-size
  :hook
  (term-mode . (lambda () (setq show-trailing-whitespace nil)))
  :bind
  ("<f8>" . multi-term-dedicated-toggle)
  :init
  (defalias 'term 'multi-term)
  :custom
  (multi-term-dedicated-select-after-open-p t)
  (multi-term-dedicated-window-height 24)
  (term-default-bg-color "#000000")
  (term-default-fg-color "#cccccc")
  :config
  (setq-default term-buffer-maximum-size 10000))


;;; NODEJS-REPL
(use-package nodejs-repl
  :ensure nodejs-repl
  :commands
  nodejs-repl)


;;; NVM
(use-package nvm
  :ensure nvm
  :commands
  nvm-use
  :functions
  nvm--installed-versions
  :custom
  ;; this bit depends on pulling this in from exec-shell,
  ;; which is done in init.el.
  (nvm-dir (getenv "NVM_DIR")))


;;; OBSIDIAN
(use-package obsidian
  :ensure obsidian
  :commands
  global-obsidian-mode
  obsidian-specify-path
  :custom
  (elgrep-data-file nil)
  :config
  (obsidian-specify-path "~/Library/Mobile Documents/iCloud~md~obsidian/Documents/Foo/")
  (global-obsidian-mode t))

(use-package vterm
  :ensure vterm
  :config
  (setq shell-file-name "/bin/zsh"
        vterm-max-scrollback 5000))

(use-package vterm-toggle
  :after vterm
  :ensure vterm-toggle
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

;;; RUST STUFF
(use-package rustic
  :ensure rustic
  :defines
  rustic-lsp-client
  :custom
  (rustic-lsp-client 'eglot))

(use-package cargo-mode
  :ensure cargo-mode
  :after rust-mode
  :hook
  (rust-mode . cargo-minor-mode)
  (rust-ts-mode . cargo-minor-mode))

;;; SASS-MODE
(use-package sass-mode
  :ensure sass-mode
  :mode "\\.s[ac]ss\\'")


;;; SCALA
(use-package ensime
  :defer ensime
  :if (file-exists-p "/opt/ensime")
  :load-path "/opt/ensime/elisp"
  :hook
  (scala-mode . ensime-scala-mode-hook))


;;; SMARTPARENS
(use-package smartparens
  :ensure smartparens
  :diminish
  :functions
  show-smartparens-global-mode
  smartparens-global-mode
  turn-off-smartparens-mode
  :bind
  ("C-M-a" . sp-beginning-of-sexp)
  ("C-M-b" . sp-backward-sexp)
  ("C-M-e" . sp-end-of-sexp)
  ("C-M-f" . sp-forward-sexp)
  ("C-M-n" . sp-next-sexp)
  ("C-M-p" . sp-previous-sexp)
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-mode 1)
  (show-smartparens-global-mode t))


;;; SVELTE-MODE
(use-package svelte-mode
  :ensure svelte-mode
  :commands
  svelte-mode
  :hook
  (svelte-mode . add-node-modules-path))


;;; TEMPLATE
(defun os/enable-template-minor-mode ()
  "Turn on `template-minor-mode' in *.tt files."
  (if (string-match "\\.tt2?\\'" buffer-file-name)
      (template-minor-mode 1)))

(use-package template-mode
  :ensure genehack-perl-elisp
  :commands
  template-minor-mode
  :hook
  (html-mode .  os/enable-template-minor-mode))


;;; TERRAFORM
(declare-function terraform-format-on-save-mode "terraform-mode")
(use-package terraform-mode
  :ensure terraform-mode
  :commands
  terraform-mode
  terraform-format-on-save-mode
  :hook
  (terraform-mode . terraform-format-on-save-mode))


;;; TREESIT
(use-package treesit
  :ensure nil
  :demand
  :commands
  treesit-font-lock-recompute-features
  :custom
  (treesit-font-lock-level 4)
  :config
  (setq major-mode-remap-alist
        '(
          (bash-mode . bash-ts-mode)
          (css-mode . css-ts-mode)
          (js2-mode . js-ts-mode)
          (json-mode . json-ts-mode)
          (python-mode . python-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (yaml-mode . yaml-ts-mode)))
  (treesit-font-lock-recompute-features))

;;; TREESIT-AUTO
(use-package treesit-auto
  :ensure treesit-auto
  :demand
  :commands
  global-treesit-auto-mode
  make-treesit-auto-recipe
  treesit-auto-add-to-auto-mode-alist
  treesit-auto-install-all
  :defines
  treesit-auto-fallback-alist
  treesit-auto-recipe-list
  :custom
  (treesit-auto-install 'prompt)
  :config
  (add-to-list 'treesit-auto-fallback-alist '(bash-ts-mode . sh-mode))
  (treesit-auto-add-to-auto-mode-alist)
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
  "Recipe for `libtree-sitter-tsx.dylib'.")
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
  "Recipe for `libtree-sitter-typescript.dylib'.")
(add-to-list 'treesit-auto-recipe-list os/typescript-treesit-auto-recipe)


;;; TYPESCRIPT-TS-MODE
(use-package ansi-color
  :defines ansi-color-compilation-filter)
(use-package compile
  :defines
  compilation-filter-hook)

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(defun os/typescript-ts-mode-setup ()
  "Set up `typescript-ts-mode' for os."
  (set (make-local-variable 'compile-command) "tsc"))

(use-package typescript-ts-mode
  :ensure typescript-ts-mode
  :mode
  "\\.ts\\'"
  :hook
  (typescript-ts-base-mode . add-node-modules-path)
  (typescript-ts-base-mode . os/typescript-ts-mode-setup))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))


;;; UNICODE-FONTS
(use-package unicode-fonts
  :ensure unicode-fonts
  :commands
  unicode-fonts-setup
  :config
  (unicode-fonts-setup))


;;; WEB-BEAUTIFY
(use-package web-beautify
  :ensure web-beautify
  :commands
  web-beautify-css
  web-beautify-js
  web-beautify-html
  :config
  (defvar css-mode-map)
  (defvar js2-mode-map)
  (defvar json-mode-map)
  (defvar sgml-mode-map)
  (defvar web-mode-map)
  (eval-after-load 'css-mode  '(define-key css-mode-map  (kbd "C-c b") 'web-beautify-css))
  (eval-after-load 'js2-mode  '(define-key js2-mode-map  (kbd "C-c b") 'web-beautify-js))
  (eval-after-load 'json-mode '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))
  (eval-after-load 'sgml-mode '(define-key sgml-mode-map (kbd "C-c b") 'web-beautify-html))
  (eval-after-load 'web-mode  '(define-key web-mode-map  (kbd "C-c b") 'web-beautify-html)))


;;; WEB-MODE
(use-package web-mode
  :ensure web-mode
  :mode
  "\\.\\(html\\|tx\\|liquid\\)\\'"
  :hook
  ((web-mode . add-node-modules-path)
   (web-mode . (lambda () (turn-off-smartparens-mode)))
   (web-mode . (lambda () (add-hook 'after-save-hook 'eslint-fix nil t))))
  :init
  (add-to-list 'safe-local-variable-values '(web-mode-code-indent-offset   . 2))
  (add-to-list 'safe-local-variable-values '(web-mode-css-indent-offset    . 2))
  (add-to-list 'safe-local-variable-values '(web-mode-markup-indent-offset . 2))
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-enable-comment-interpolation t)
  (web-mode-markup-indent-offset 2))

;; in order to be able to use web-mode for *.vue files, but only run
;; the Vetur vls server for *.vue files and not _all_ web-mode files,
;; i made this new mode that's just a copy of web-mode, and then i map
;; the mode to vls in eglot. this is somewhat convoluted, but it works.
(define-derived-mode genehack-vue-mode web-mode "ghVue"
  "A major mode derived from `web-mode', for editing .vue files with LSP support.")
(add-to-list 'auto-mode-alist '("\\.vue\\'" . genehack-vue-mode))


;;; WHICH-KEY
(use-package which-key
  :ensure which-key
  :diminish
  :functions
  which-key-mode
  :config
  (which-key-mode))


;;; YAML-MODE
(use-package yaml-mode
  :ensure yaml-mode
  :commands
  yaml-mode
  :defines
  yaml-mode-map
  yaml-ts-mode-map
  :mode
  "\\.ya?ml\\'"
  :bind
  (:map yaml-mode-map ("RET" . newline-and-indent))
  (:map yaml-ts-mode-map ("RET" . newline-and-indent))
  :config
  (add-to-list 'safe-local-variable-values '(yaml-indent-offset . 4)))



;; https://github.com/clojure-emacs/clojure-mode
(use-package clojure-mode
  :ensure clojure-mode)


;; extra syntax highlighting for clojure
(use-package clojure-mode-extra-font-locking
  :after (clojure-mode)
  :ensure t)

;;;;
;; Cider
;;;;

;; integration with a Clojure REPL
;; https://github.com/clojure-emacs/cider

(use-package cider
  :defines
  nrepl-popup-stacktraces
  :ensure t)

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



;;; PRISMA
(use-package prisma-ts-mode
  :ensure t)

(add-to-list
 'treesit-language-source-alist
 '(prisma "https://github.com/victorhqc/tree-sitter-prisma"))


;;; Rest client
(use-package restclient
  :ensure restclient)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'misc-packages)
;;; misc-packages.el ends here
