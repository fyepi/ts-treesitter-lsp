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


(add-to-list 'use-package-keywords :os/bind t)

(defun use-package-normalize/:os/bind (name keyword args)
  "Custom use-keyword :os/bind.
I use this to provide something similar to ':bind', but with two
additional features that I miss from the default implementation:

Uses NAME KEYWORD ARGS

1. Integration with 'evil-define-key', so I can extend the keymap
   declaration to specify one or more evil states that the
   binding should apply to.

2. The ability to detect keymaps that aren't defined as prefix
   commands. This allows me to define a binding to a keymap
   variable, eg. maybe I want '<leader>h' to trigger 'help-map'.
   This fails using the default ':bind', meaning that I have to
   fall back to calling 'bind-key' manually if I want to assign a
   prefix.

The expected form is slightly different to 'bind':

((:map (KEYMAP . STATE) (KEY . FUNC) (KEY . FUNC) ...)
 (:map (KEYMAP . STATE) (KEY . FUNC) (KEY . FUNC) ...) ...)

STATE is the evil state. It can be nil or omitted entirely. If
given, it should be an argument suitable for passing to
'evil-define-key' -- meaning a symbol like 'normal', or a list
like '(normal insert)'."
  (setq args (car args))
  (unless (listp args)
    (use-package-error ":os/bind expects ((:map (MAP . STATE) (KEY . FUNC) ..) ..)"))
  (dolist (def args args)
    (unless (and (eq (car def) :map)
                 (consp (cdr def))
                 (listp (cddr def)))
      (use-package-error ":os/bind expects ((:map (MAP . STATE) (KEY . FUNC) ..) ..)"))))

(defun use-package-handler/:os/bind (name _keyword args rest state)
  "Handler for ':os/bind' use-package extension.
See 'use-package-normalize/:os/bind' for docs."
  (let ((body (use-package-process-keywords name rest
                (use-package-plist-delete state :os/bind))))
    (use-package-concat
     `((with-eval-after-load ',name
         ,@(mapcan
            (lambda (entry)
              (let ((keymap (car (cadr entry)))
                    (state (cdr (cadr entry)))
                    (bindings (cddr entry)))
                (mapcar
                 (lambda (binding)
                   (let ((key (car binding))
                         (val (if (and (boundp (cdr binding)) (keymapp (symbol-value (cdr binding))))
                                  ;; Keymaps need to be vars without quotes
                                  (cdr binding)
                                ;; But functions need to be quoted symbols
                                `(quote ,(cdr binding)))))
                     ;; When state is provided, use evil-define-key. Otherwise fall back to bind-key.
                     (if state
                         `(evil-define-key ',state ,keymap (kbd ,key) ,val)
                       `(bind-key ,key ,val ,keymap))))
                 bindings)))
            args)))
     body)))


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
(setq read-process-output-max (* 10 1024 1024)) ;; 10mb
(setq gc-cons-threshold 200000000)


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
    "builtins"
    "straight"
    "init"
    "treesitter"
    "completions"
    "clojure"
    "keys"
    "linters"
    "misc-functions"
    "misc-highlights"
    "misc-packages"
    "themes"
    "ai"
    "spelling"
    "3e"
    "dotenv-mode")
  "List of modules to load on startup.")

(dolist (pkg os/module-list)
  (if (file-readable-p (concat os/emacs-config-dir pkg ".el"))
      (load-library pkg)))


;;; DEFAULT FACE
;;;; If you don't set this early on, sometimes things get wonky.
(set-face-attribute 'default nil
                    :font "Iosevka"
                    :height 140
                    :weight 'normal)
(set-face-attribute 'fixed-pitch nil
                    :font "Iosevka"
                    :height 130
                    :weight 'normal)
(set-face-attribute 'variable-pitch nil
                    :font "Iosevka"
                    :height 130
                    :weight 'normal)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
                    :slant 'italic)

(set-face-attribute 'mode-line nil :height 140 :family "Iosevka")

(provide 'init)
;;; init.el ends here
