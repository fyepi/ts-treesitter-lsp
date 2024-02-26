;;; builtins.el -- Customizations of built-in (non-third party) options.  ;; -*- lexical-binding: t; -*-

;;; Commentary:
;;;; Large packages (eg Gnus, ERC, etc.) should get dedicated files but
;;;; for standard stuff where one or two options get frobbed, this is
;;;; the place.

;;; Code:
;;; ANSI-MODE FOR SHELLS
(ansi-color-for-comint-mode-on)


;;; AUTO CREATE DIRECTORIES
;;;; after <http://atomized.org/2008/12/emacs-create-directory-before-saving/>
(defun os/before-save-hook ()
  "My customizations for `before-save-hook'."
  (or (file-exists-p (file-name-directory buffer-file-name))
      (make-directory (file-name-directory buffer-file-name) t)))
(add-hook 'before-save-hook 'os/before-save-hook)


;;; AUTO-SAVES AND BACKUPS
(eval-when-compile (defvar os/emacs-tmp-dir))
(defvar os/backup-dir (concat os/emacs-tmp-dir "saves/" )
  "Place to put file backups.")
(setq auto-save-list-file-prefix (concat os/emacs-tmp-dir "auto-save-list/.saves-"))
(setq auto-save-file-name-transforms `((".*" ,os/emacs-tmp-dir t)))
(setq backup-by-copying t)
(setq backup-directory-alist `((".*" . ,os/backup-dir)))
(setq create-lockfiles nil)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)


;;; BUFFER-MODE
(defun os/set-up-buffer-mode ()
  "My customizations for `Buffer-menu-mode'."
  (setq show-trailing-whitespace nil))
(add-hook 'Buffer-menu-mode-hook 'os/set-up-buffer-mode)


;;; CALENDAR
(defun os/set-up-calendar-mode ()
  "My customizations for `calendar-mode'."
  (setq show-trailing-whitespace nil))
(use-package calendar
  :hook
  (calendar-mode . os/set-up-calendar-mode)
  :custom
  (calendar-mark-holidays-flag t))


;;; CONVERT LINE ENDINGS
;;;; from http://www.emacswiki.org/emacs/EndOfLineTips
(add-hook 'find-file-hook 'os/find-file-check-line-endings)
(defun os/dos-file-endings-p ()
  "Predicate for whether current buffer is in DOS mode."
  (string-match "dos" (symbol-name buffer-file-coding-system)))
(defun os/find-file-check-line-endings ()
  "Convert DOS file to Unix."
  (when (os/dos-file-endings-p)
    (set-buffer-file-coding-system 'undecided-unix)
    (set-buffer-modified-p nil)))


;;; CURSOR
(setq-default cursor-type 'box)


;;; DEBUGGING
(setq-default warning-minimum-level :error)


;;; DESKTOP
(desktop-save-mode 1)


;;; DIRED
;;;; having all-the-icons mode on is pretty, but having the icons in
;;;; the buffer fucks up wdired something fierce. let's advise the
;;;; functions that take us into and out of wdired mode so they remove
;;;; the icons and then put them back.
(defun os/disable-all-the-icons ()
  "Disable all-the-icons."
  (all-the-icons-dired-mode 0))
(defun os/restore-all-the-icons ()
  "Restore all-the-icons."
  (all-the-icons-dired-mode 1))
(advice-add 'wdired-change-to-wdired-mode :before #'os/disable-all-the-icons)
(advice-add 'wdired-finish-edit           :after  #'os/restore-all-the-icons)
(advice-add 'wdired-abort-changes         :after  #'os/restore-all-the-icons)

;;;; http://whattheemacsd.com//setup-dired.el-02.html
(defun dired-back-to-top ()
  "Jump to the top file in a Dired buffer."
  (interactive)
  (goto-char (point-min))
  ;; because the number of header lines varies depending on whether
  ;; mode info is shown or hidden, find the double-dot directory entry
  ;; and go forward one line -- heuristic, but will always work.
  (search-forward "..")
  (dired-next-line 1))

(defun dired-jump-to-bottom ()
  "Jump to the last file in a Dired buffer."
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1))

(use-package dired
  :after (all-the-icons all-the-icons-dired)
  :commands
  dired
  dired-jump
  dired-next-line
  :hook
  ((dired-mode . all-the-icons-dired-mode)
   (dired-mode . dired-hide-details-mode))
  :bind
  ("C-c d"   . dired-jump)
  ("C-c C-d" . dired-jump)
  (:map dired-mode-map
        ("E"                         . wdired-change-to-wdired-mode)
        ([remap beginning-of-buffer] . dired-back-to-top)
        ([remap end-of-buffer]       . dired-jump-to-bottom)))


;;; DISABLE / ENABLE
(put 'downcase-region 'disabled nil)
(put 'overwrite-mode  'disabled t)
(put 'upcase-region   'disabled nil)


;;; ELDOC
(use-package eldoc
  :defer t
  :diminish
  :init
  (global-eldoc-mode))


;;; EXECUTABLE-UPON-SAVE MAGIC
;;;; from <http://www.emacswiki.org/cgi-bin/wiki/MakingScriptsExecutableOnSave>
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)


;;; FFAP
(use-package ffap
  :config
  (ffap-bindings))


;;; FLYSPELL
(use-package flyspell
  :diminish)


;;; FONT-LOCK
(use-package font-lock
  :custom
  (jit-lock-stealth-time 5)
  (jit-lock-defer-contextually t)
  (jit-lock-stealth-nice 0.5)
  :config
  (global-font-lock-mode 1)
  (setq-default font-lock-maximum-decoration t)
  (setq-default font-lock-maximum-size nil))


;;; FRAME RESIZE
;;;; see https://www.reddit.com/r/emacs/comments/f3ed3r/how_is_doom_emacs_so_damn_fast/fhicvbj/
(setq frame-inhibit-implied-resize t)


;;; GENERAL INDENTATION RELATED OPTIONS
(setq-default indent-tabs-mode nil)
(setq sentence-end-double-space nil)
(setq tab-always-indent 'complete)

(defvar modes-for-indentation-munging
  '(c++-mode
    c-mode
    cperl-mode
    emacs-lisp-mode
    go-mode
    js-mode
    js2-mode
    objc-mode
    perl-mode
    python-mode
    rspec-mode
    ruby-mode
    scala-mode
    terraform-mode
    typescript-ts-mode
    tsx-ts-mode
    web-mode)
  "List of modes to set up to do indent-on-paste.
Also remove-leading-whitespace-on-kill-line tricks")

;;;; based on <http://www.emacswiki.org/emacs-en/AutoIndentation>
(defun os/trim-on-kill (&optional _ARG)
  "Remove excess white space when killing newlines in configured modes.
Ignores `ARG'."
  (if (member major-mode modes-for-indentation-munging)
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))
(advice-add 'kill-line :before #'os/trim-on-kill)


;;; GLOBAL AUTO-REVERT
(use-package autorevert
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  :config
  ;; Also auto refresh dired, but be quiet about it
  (global-auto-revert-mode t))


;;; HTML-MODE
;;;; `html-mode' is actually defined in `sgml-mode.el'
(use-package sgml-mode
  :hook
  (html-mode . turn-off-auto-fill)
  :flymake-hook
  (html-mode (flymake-collection-html-tidy)))


;;; IBUFFER
(use-package ibuffer
  :bind
  ("C-x M-b" . ibuffer)
  :custom
  (ibuffer-default-sorting-mode 'major-mode))


;;; KEYSTROKE ECHO
(setq echo-keystrokes 0.1)


;;; LINE NUMBERS
(column-number-mode 1)


;;; MAC STUFF
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil)
  (setq browse-url-browser-function 'browse-url-default-macosx-browser)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))


;;; MESSAGE LOG
(setq message-log-max 5000)


;;; (MENU/SCROLLBAR/TOOLBAR)-MODE
(if (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))


;;; MOUSE WHEEL
(if (fboundp 'mouse-wheel-mode) (mouse-wheel-mode -1))


;;; NXML-MODE
(use-package nxml-mode
  :mode "\\.\\(rng\\|rss\\|xml\\|xsd\\|xslt\\)\\'"
  :magic "<\\?xml "
  :custom
  (nxml-bind-meta-tab-to-complete-flag nil)
  :init
  (fset 'xml-mode 'nxml-mode))


;;; ORG MODE
(use-package org
  :disabled t
  :commands org-directory
  :defines
  org-agenda-custom-commands
  org-agenda-files
  org-capture-templates
  org-default-notes-file
  org-log-done
  org-refile-targets
  org-return-follows-link
  org-tag-alist
  org-todo-keywords
  :config
  (setq org-agenda-custom-commands
        '(("a" "active" todo "ACTIVE")
          ("n" "next actions" tags-todo "NEXT")
          ("w" "waiting for" todo "WAITING")))
  (setq org-agenda-files '("~/org" "~/org/home" "~/org/oss" "~/org/work"))
  (setq org-capture-templates
        '(("t" "todo" entry (file+headline "" "* INBOX") "** TODO %?\n %i\n %a")))
  (setq org-default-notes-file (concat org-directory "/jfdi.org"))
  (setq org-log-done 'time)
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 2))))
  (setq org-return-follows-link t)
  (setq org-tag-alist '(("@work" . ?w) ("@home" . ?h) ("@oss" . ?o) ("NEXT" . ?n)))
  (setq org-todo-keywords '((sequence "TODO(t)" "ACTIVE(a)" "|" "DONE(d!)")
                            (sequence "WAITING(w!/@)" "|")))
  (defun jfdi ()
    "JFDI!"
    (interactive)
    (find-file "~/org/jfdi.org")))


;;; PAREN MATCH
(use-package paren
  :custom
  (show-paren-style 'expression)
  :config
  (setq show-paren-context-when-offscreen 'overlay)
  (show-paren-mode t))


;;; PROG MODE
(use-package prog-mode
  :hook
  (prog-mode . flyspell-prog-mode))


;;; PS PRINT
(setq ps-print-color-p nil)


;;; SAVE-HIST
(use-package savehist
  :custom
  (savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (savehist-file (expand-file-name "savehist" os/emacs-tmp-dir))
  :config
  (savehist-mode t))


;;; SAVEPLACE
(use-package saveplace
  :custom
  (save-place-file (expand-file-name "saveplace" os/emacs-tmp-dir))
  :config
  (save-place-mode))


;;; SCRATCH-BUFFER
(setq initial-scratch-message ";; this is my emacs. there are many like it, but this one is mine.\n\n")
(defalias 'os/create-scratch-buffer 'scratch-buffer)


;;; SERVER
(server-start)


;;; SIZE INDICATION MODE
(size-indication-mode t)


;;; SPELL CHECKING
(defvar os/found-spelling-program nil
  "Boolean indicating if a spelling program was found in the variable `exec-path'.")

;;;; (note that exec-path probably needs to be munged before this is run)
(defun os/find-in-exec-path (program)
  "Find PROGRAM in variable `exec-path'."
  (let ((found nil))
    (dolist (path exec-path)
      (if (file-exists-p (concat path "/" program))
          (setq found t)))
    found))

(defun os/spelling-not-found ()
  "Display message when *spell program can't be found."
  (interactive)
  (message "Spell check not enabled; neither aspell nor ispell found in path."))

(use-package ispell
  :custom
  (ispell-silently-savep t)
  :init
  (if (os/find-in-exec-path "aspell")
      (progn
        (setq-default ispell-program-name "aspell")
        (setq ispell-extra-args '("--sug-mode=ultra"))
        (setq os/found-spelling-program t))
    (if (os/find-in-exec-path "ispell")
        (progn
          (setq-default ispell-program-name "ispell")
          (setq ispell-extra-args '("-W 3"))
          (setq os/found-spelling-program t))))
  (if (eq os/found-spelling-program t)
      (progn
        (autoload 'ispell-word   "ispell" "check word spelling."   t)
        (autoload 'ispell-region "ispell" "check region spelling." t)
        (autoload 'ispell-buffer "ispell" "check buffer spelling." t)
        (require 'flyspell))
    (progn
      (defalias 'ispell-word   'os/spelling-not-found)
      (defalias 'ispell-region 'os/spelling-not-found)
      (defalias 'ispell-buffer 'os/spelling-not-found))))


;;; TEXT-MODE
(declare-function filladapt-mode "fillapdapt")
(defun os/set-up-text-mode ()
  "My customizations for `text-mode'."
  (require 'filladapt)
  (auto-fill-mode 1)
  (filladapt-mode 1)
  (if (eq os/found-spelling-program t)
      (flyspell-mode 1)))
(use-package text-mode
  :hook
  (text-mode . os/set-up-text-mode))


;;; TITLE BARS
(setq frame-title-format "<%b> == (%f) [mode: %m]")


;;; TRANSIENT MARK MODE
(transient-mark-mode 1)


;;; TRASH
(setq delete-by-moving-to-trash t)


;;; UNIQUIFY
(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "/")
  (uniquify-after-kill-buffer-p t))


;;; UTF8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


;;; VC
(setq vc-follow-symlinks t)


;;; VIEW MODE
;;;; use view-mode in read-only buffers (https://karthinks.com/software/batteries-included-with-emacs/)
(setq view-read-only t)


;;; WHITESPACE
(use-package whitespace
  :custom
  (whitespace-style
   '(face tabs spaces trailing lines-tail
          space-before-tab newline indentation empty
          space-after-tab space-mark tab-mark newline-mark)))


;;; YANK
(setq-default mouse-yank-at-point t)


;;; YES-OR-NO
(defalias 'yes-or-no-p 'y-or-n-p)


(provide 'builtins)
;;; builtins.el ends here
