;; init.el -- master configuration file ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
;; DEBUG
;; (setq debug-on-error t)
;; (setq debug-on-quit t)


;;; VARIABLES
;;;; if you're not me, you probably want to change this to point to
;;;; where you put your checkout of the github repo:
(defvar os/emacs-dir (expand-file-name "~/.emacs.d/")
  "Directory containing working copy of Emacs config repo.")

(defvar os/emacs-config-dir (concat os/emacs-dir "etc/")
  "Sub-directory containing config files.")

(add-to-list 'load-path os/emacs-config-dir)

(defvar os/emacs-tmp-dir (concat os/emacs-dir "tmp/")
  "Scratch space for stuff...")

;;;; I generally don't use the Customize interface, but sometimes
;;;; things get added there. Setting this means the file is under
;;;; revision control, so if something touches it, I'll notice (and
;;;; then I can move stuff to where it really goes and empty out
;;;; custom.el again...)
(setq custom-file (concat os/emacs-config-dir "custom.el"))
(load custom-file)


;;; DEFAULT DIRECTORY
;;;; This makes `find-file' in a buffer like `*scratch*' default to
;;;; $HOME instead of `/'
(setq default-directory "~/")


;;; ELPA
(require 'package)
(setq package-user-dir (concat os/emacs-dir "elpa"))
(setq package-native-compile t)

;;;; initialize ELPA, creating package directory if necessary
;;;; (and complaining if we're blocked by a file...)
(if (file-exists-p package-user-dir)
    (if (file-directory-p package-user-dir)
        (package-initialize)
      (error "ELPA package dir creation blocked by file"))
  (make-directory package-user-dir))


;;; PRIVATE CONFIG
;;;; Again, you might want to change this path to a private file of
;;;; your own. This is a place to put variables that set passwords,
;;;; email addresses, and other stuff that you don't want in a public
;;;; github repo...
(defvar os/private-config-file "~/.emacs.d/emacs-private.el"
  "File with configuration info that can't be in public repository.")
(if (file-readable-p os/private-config-file)
    (progn
      (load-library os/private-config-file)
      (message "Loaded private config")))


;;; PACKAGE
(eval-after-load "package"
  '(setq package-archives
         '(("gene-melpa" . "https://melpa.genehack.net/packages/")
           ("melpa"    . "https://melpa.org/packages/")
           ("gnu"      . "https://elpa.gnu.org/packages/"))))

(defvar os/packages-refreshed nil
  "Flag for whether package lists have been refreshed yet.")

(defun os/package-refresh (&rest _args)
  "Refresh package metadata, if needed."
  (unless (eq os/packages-refreshed t)
    (progn
      (message "Refreshing package metadata...");
      (package-refresh-contents)
      (setq os/packages-refreshed t))))
(advice-add 'package-install :before #'os/package-refresh)

;;;; we need use-package -- it'll take care of installing everything else
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)


;;; GARBAGE COLLECTION MAGIC HACK
;;;; speeds startup?
(use-package gcmh
  :ensure gcmh
  :demand
  :diminish
  :functions
  gcmh-mode
  :init
  (gcmh-mode 1))

;;;; per https://github.com/emacs-lsp/lsp-mode#performance
(setq read-process-output-max (* 2 1024 1024)) ;; 2mb
(setq gc-cons-threshold 100000000)


;;; EXEC-PATH-FROM-SHELL
(setenv "PLENV_ROOT" "/opt/plenv")
(use-package exec-path-from-shell
  :ensure exec-path-from-shell
  :demand
  :functions
  exec-path-from-shell-initialize
  :init
  ;; FIXME seeing if this does anything... (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize)
  :custom
  (exec-path-from-shell-variables
   '(
     "CARGO_HOME"
     "GOPATH"
     "GOROOT"
     "MANPATH"
     "NVM_DIR"
     "PATH"
     "PLENV_ROOT"
     "RUSTUP_HOME"
     "SSH_AGENT_PID"
     "SSH_AUTH_SOCK"
     )))


;;; MODULES
;;;; All the rest of the config is split out into individual files, for
;;;; ease of use.
(defvar os/module-list
  '(
    "secrets"
    "builtins"
    "straight"
    "init"
    "treesitter"
    "completions"
    "keys"
    "linters"
    "misc-functions"
    "misc-highlights"
    "misc-packages"
    "themes"
    "css-in-js-mode"
    "tsx-mode")
  "List of modules to load on startup.")

(dolist (pkg os/module-list)
  (if (file-readable-p (concat os/emacs-config-dir pkg ".el"))
      (load-library pkg)))


;;; DEFAULT FACE
;;;; If you don't set this early on, sometimes things get wonky.
(set-face-attribute 'default nil
                    :font "JetBrainsMono Nerd Font"
                    :height 130
                    :weight 'semibold)
(set-face-attribute 'fixed-pitch nil
                    :font "JetBrainsMono Nerd Font"
                    :height 130
                    :weight 'semibold)
(set-face-attribute 'variable-pitch nil
                    :font "JetBrainsMono Nerd Font"
                    :height 130
                    :weight 'semibold)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
                    :slant 'italic)

(set-face-attribute 'mode-line nil :height 120 :family "JetBrainsMono Nerd Font")

(provide 'init)
;;; init.el ends here
