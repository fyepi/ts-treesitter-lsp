;; misc-functions.el --- various helper functions  ;; -*- lexical-binding: t; -*-

;;; Commentary:
;;;; This is for useful functions that don't have anymore more
;;;; obviously correct to live. NOTE: DO NOT ADD STUFF THAT USES
;;;; `use-package' HERE; THAT SHOULD BE ELSEWHERE!

;;; Code:
;;;; functions in reasonable alpha order by name, please, with
;;;; associated `defvar' (&tc.) calls preceding and associated
;;;; `bind-key' calls following.

;;; CHOMP TRAILING WHITE SPACE HELPER
;;;; from https://www.emacswiki.org/emacs/ElispCookbook#toc6
(defun genehack/chomp-end (str)
  "Chomp tailing white space from STR."
  (replace-regexp-in-string
   (rx (* (any " \t\n")) eos)
   ""
   str))


;;; DIFF-CURRENT-BUFFER-WITH-FILE
(defun os/diff-current-buffer-with-file ()
  "Show diff between current buffer contents and file on disk."
  (interactive)
  (diff-buffer-with-file (current-buffer)))
(bind-key "C-x =" #'os/diff-current-buffer-with-file)


;;; FILE / BUFFER FINDING, HANDLING, AND KLLIN
(declare-function projectile-project-p "projectile")
(declare-function projectile-find-file "projectile")


(defun os/find-file (arg)
  "Pick `projectile-file-file` or `find-file` (Force latter w/ARG).
Decision is based on the value of `projectile-project-p`.  If
given a prefix arg ARG, unconditionally use `find-file`."
  (interactive "P")
  (if (and (projectile-project-p) (null arg))
      (projectile-find-file)
    (call-interactively 'find-file)))
(bind-key "C-x f"   #'os/find-file)
(bind-key "C-x C-f" #'os/find-file)

(defun os/kill-this-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer nil))
(bind-key "C-x K" #'os/kill-this-buffer)



;;; GIT/MAGIT BITS
(declare-function magit-get-current-branch  "magit")
(declare-function magit-git-string          "magit")
(declare-function magit-status-setup-buffer "magit")
(declare-function projectile-project-root   "projectile")

(defvar os/browse-file-in-repo-website-sub-url-lookup-map
  '(("gitea" . "/src/branch")
    ("git"   . "/blob"))
  "Alist mapping between remote git user name...
and sub-url to insert when building URL to file and line.")

(defvar os/git-executable (executable-find "git")
  "Path to active `git' executable.")

(defun os/browse-file-in-repo-website ()
  "Open the web page for a file in a Git repository."
  (interactive)
  (let* (;; "gitea@git.genehack.net:os/emacs.git"
         (remote (magit-git-string "remote" "get-url" "origin"))
         ;; "main"
         (branch (magit-get-current-branch))
         ;; ("gitea" "git.genehack.net:os/emacs.git")
         (parts (split-string remote "@"))
         ;; "git.genehack.net:os/emacs.git" -> "git.genehack.net/os/emacs"
         (base-url (replace-regexp-in-string ":" "/" (replace-regexp-in-string ".git$" "" (car (cdr parts)))))
         (sub-url (alist-get (car parts) os/browse-file-in-repo-website-sub-url-lookup-map "" nil 'equal))
         (file (file-relative-name buffer-file-name (projectile-project-root)))
         (line (number-to-string (line-number-at-pos)))
         (url (concat "https://" base-url sub-url "/" branch "/" file "#L" line)))
    (browse-url url)))
(bind-key "C-c C-l" #'os/browse-file-in-repo-website)

(defun os/git-blame-for-line ()
  "Show git blame for current line."
  (interactive)
  (defvar blame-out "")
  (let ((blame-line (line-number-at-pos (point)))
        (blame-file (buffer-file-name)))
    (setq blame-out (shell-command-to-string
                     (format "~/bin/git-blame-from-line-num %s %s" blame-line blame-file)))
    (with-output-to-temp-buffer "*git blame*" (princ blame-out))))
(bind-key "C-c l" #'os/git-blame-for-line)

;;;; from http://whattheemacsd.com/setup-magit.el-01.html
(defun os/magit-fullscreen (orig-fxn &rest args)
  "Full screen `magit-status'.
When installed via `advice-add', ORIG-FXN will be `magit-status'
and ARGS will be any args it originally got."
  (window-configuration-to-register :magit-fullscreen)
  (apply orig-fxn args)
  (delete-other-windows))
(advice-add 'magit-status :around #'os/magit-fullscreen)

(defun os/magit-key (arg)
  "Call `magit-status', or with ARG call `os/magit-status-with-prompt'."
  (interactive "P")
  (if arg
      (call-interactively 'os/magit-status-with-prompt)
    (call-interactively 'magit-status)))
(bind-key "C-c C-g" #'os/magit-key)

(defun os/magit-quit-session ()
  "Restore the previous window configuration and kill the magit buffer."
  (interactive)
  (kill-buffer)
  (if (assoc :magit-fullscreen register-alist)
      (jump-to-register :magit-fullscreen)))

(if os/git-executable
    (defun os/magit-status-with-prompt (dir)
      "Prompt for git repo path then call magit-status on it."
      (interactive "Dgit repo: ")
      (magit-status-setup-buffer dir))
  (defun os/magit-status-with-prompt ()
    "Stub function for when git isn't available"
    (interactive)
    (message "Unable to find a git binary; magit is unavailable.")))



;;; JAVASCRIPT / NODE / TYPESCRIPT STUFF
(declare-function nvm--installed-versions "nvm")
(declare-function nvm-use                 "nvm")

(defvar os/node-version "" "Version of Node to use as read from .nvmrc file.")
(defvar os/nvmrc-file ".nvmrc" "Path to nvmrc file relative to project root.")

(defun os/js2-insert-debug (var)
  "Prompt for VAR and then insert debug statement."
  (interactive "sVar: ")
  (save-excursion
    (insert
     "const util = require(\"util\");console.log(util.inspect("
     var
     ", { showHidden: false, depth: null }));"
     )))

(defun os/node-project-setup ()
  "Use nvm to set active Node version if .nvmrc file exists in project root."
  (interactive)
  (if (file-exists-p os/nvmrc-file)
      (progn
        (setq os/node-version (os/chomp-end
                                     (with-temp-buffer
                                       (insert-file-contents os/nvmrc-file)
                                       (buffer-string))))
        (os/nvm os/node-version)
        (message "Set up to use node version %s" os/node-version))))

(defun os/nvm (version)
  "Reconfigure $PATH and function `exec-path' to use a particular Node.js VERSION.
\(Via nvm.) When called with a prefx argument, use the highest
Node version locally available."
  (interactive
   (list (if (not (consp current-prefix-arg))
             (read-string "Version: ")
           (car (car (last (nvm--installed-versions)))))))
  (nvm-use version)
  (setq exec-path (parse-colon-path (getenv "PATH"))))


;;; LINE NUMBERS WITH M-G
;; from http://whattheemacsd.com//key-bindings.el-01.html
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (defvar goto/line 0)
  (unwind-protect
      (progn
        ;; display-line-numbers is Emacs 26 specific; if this gives
        ;; you problems, conditionalize between it and linum-mode
        ;; depending on Emacs version
        (display-line-numbers-mode 1)
        (setq goto/line (read-number "Goto line: "))
        (goto-char (point-min))
        (forward-line (1- goto/line)))
    (display-line-numbers-mode -1)))
(bind-key "M-g" #'goto-line-with-feedback)


;;; MACRO
;;;; after <http://www.emacswiki.org/cgi-bin/wiki.pl?MacroKey>
(defun os/macro-dwim (arg)
  "DWIM keyboard macro recording and executing.
If provided, ARG will be passed as a macro execution repeat count."
  (interactive "P")
  (if defining-kbd-macro
      (if arg
          (end-kbd-macro arg)
        (end-kbd-macro))
    (if last-kbd-macro
        (call-last-kbd-macro arg)
      (start-kbd-macro arg))))
(bind-key "<f6>" #'os/macro-dwim)

(defun os/macro-clear ()
  "Clear out the last keyboard macro."
  (interactive)
  (setq last-kbd-macro nil)
  (message "Last keyboard macro cleared."))
(bind-key "<f7>" #'os/macro-clear)


;;; MARKDOWN STUFF
(declare-function html-to-markdown-string "html-to-markdown")
(defun os/html-to-markdown-on-region (beg end)
  "Convert the text from BEG to END from HTML into markdown."
  (interactive "r")
  (let ((markdown (html-to-markdown-string (buffer-substring beg end))))
    (delete-active-region)
    (insert markdown)))


;;; NOOP
(defun os/noop nil "..." (interactive))
;; the things bound to os/noop are shadowed by the hardwired
;; multimedia keys on my keyboard -- but sometimes i use the keyboard
;; on my laptop, and i don't want something to happen if i
;; accidentally hit one of those keys there. i *could* just unbind
;; them, but then some other code might think they're available -- and
;; they're not, they're all MINE!!! BWAHAHAHAH.
;;
(global-set-key (kbd "<f5>")      #'os/noop)
(global-set-key (kbd "<f9>")      #'os/noop)
(global-set-key (kbd "<f10>")     #'os/noop)
(global-set-key (kbd "<f11>")     #'os/noop)

;;; OPEN LINE
;;;; from http://whattheemacsd.com//editing-defuns.el-01.html
(defun open-line-below ()
  "Open a line below the current line."
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))
(bind-key "C-<return>" #'open-line-below)


(defun open-line-above ()
  "Open a line above the current line."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))
(bind-key "M-<return>" #'open-line-above)


;;; OPEN WITH
;;;; from http://emacsredux.com/blog/2013/03/27/open-file-in-external-program/
(defun open-with ()
  "Open the underlying file of a buffer in an external program."
  (interactive)
  (when buffer-file-name
    (shell-command (concat
                    (if (eq system-type 'darwin)
                        "open"
                      (read-shell-command "Open current file with: "))
                    " "
                    buffer-file-name))))
(bind-key "C-c o" #'open-with)


;;; PAREN-BOUNCE
;;;; originally ganked from <http://elfs.livejournal.com/1216037.html>
(defun genehack/paren-bounce ()
  "Bounce from one paren to the matching paren."
  (interactive)
  (let ((prev-char (char-to-string (preceding-char)))
        (next-char (char-to-string (following-char))))
    (cond ((string-match "[[{(<\"']" next-char) (forward-sexp 1))
          ((string-match "[\]})>\"']" prev-char) (backward-sexp 1))
          (t (error "%s" "Not an expression boundary.")))))
(bind-key "C-%" #'genehack/paren-bounce)
(bind-key "C-5" #'genehack/paren-bounce)
                              
(defun os/set-frame-font-size (&optional font-size)
  "Change frame font size to FONT-SIZE.
If no FONT-SIZE provided, reset the size to its default variable."
  (let ((font-size
         (or font-size
             (car (get 'os/font-size 'standard-value)))))
    (customize-set-variable 'os/font-size font-size)
    (set-frame-font
     (format "%s %d" os/font-name font-size) nil t)))

(defun os/global-text-scale-increase ()
  "Increase frame font by one."
  (interactive)
  (os/set-frame-font-size (+ os/font-size 5)))
(bind-key "M-+" #'os/global-text-scale-increase)
(bind-key "M-=" #'text-scale-increase)

(defun os/global-text-scale-decrease ()
  "Decrease frame font by one."
  (interactive)
  (os/set-frame-font-size (- os/font-size 1)))
(bind-key "M-_" #'os/global-text-scale-decrease)
(bind-key "M--" #'text-scale-decrease)

(defun os/global-text-scale-reset ()
  "Reset frame font."
  (interactive)
  (os/set-frame-font-size))
(bind-key "M-0" #'os/text-scale-default)
(bind-key "M-)" #'os/global-text-scale-reset)


;;; TOGGLE-BOL
(defun os/bol-toggle ()
  "Toggle between beginning of indent and beginning of line."
  (interactive)
  (let ((os/bol-command-name "os/bol-toggle"))
    (setq this-command os/bol-command-name)
    (cond ((equal (point) (line-beginning-position)) (back-to-indentation))
          ((equal last-command os/bol-command-name) (move-beginning-of-line nil))
          (t (back-to-indentation)))))
(bind-key "C-a" #'os/bol-toggle)


;;; URL ENCODING
;;;; based on http://twitter.com/#!/OvidPerl/status/28076709865586688
(defun os/unescape_uri (begin end)
  "URI unescape region between BEGIN and END."
  (interactive "r")
  (shell-command-on-region
   begin end
   "perl -MURI::Escape -e 'print URI::Escape::uri_unescape(do { local $/; <STDIN> })'"
   'current-buffer t))


;;; UUID
(defun os/uuid ()
  "Insert a newly generated UUID at point.
Also copies the new UUID to the clipboard."
  (interactive)
  (let ((uuid (downcase
               (replace-regexp-in-string
                (rx (* (any " \t\n")) eos) ""
                (shell-command-to-string "uuidgen")))))
    (kill-new uuid)
    (insert uuid)))


;;; WHITE SPACE RELATED STUFF
(setq-default show-trailing-whitespace t)
(setq-default require-final-newline t)

(defun os/kill-space-forward ()
  "Delete white space to the right of point."
  (interactive)
  (save-excursion
    (let* ((here (point)))
      (skip-chars-forward " ")
      (delete-region here (point)))))
(bind-key "M-SPC" #'os/kill-space-forward)

(defvar os/strip-trailing-whitespace-in-these-modes
  '(
    c++-mode
    clojure-mode
    cperl-mode
    csharp-mode
    css-mode
    emacs-lisp-mode
    js-mode
    js2-mode
    lisp-mode
    markdown-mode
    org-mode
    python-base-mode
    ruby-mode
    scala-mode
    swift-mode
    terraform-mode
    tt-mode
    typescript-ts-mode
    yaml-mode
    web-mode
    )
  "List of modes where trailing whitespace should be stripped when saving files.")
;;;; but we don't want to highlight whitespace in the minibuffer...
(add-hook 'minibuffer-setup-hook
          (lambda () (setq-local show-trailing-whitespace nil)))
(add-hook 'completion-list-mode-hook
          (lambda () (setq-local show-trailing-whitespace nil)))

(defun os/set-up-whitespace-strip-in-these-modes ()
  "Set up whitespace stripping.
Applies to the modes in os/strip-trailing-whitespace-in-these-modes."
  (if (member major-mode os/strip-trailing-whitespace-in-these-modes)
      (os/strip-whitespace)))
(add-hook 'before-save-hook 'os/set-up-whitespace-strip-in-these-modes)

;;;; inspired by http://whattheemacsd.com/buffer-defuns.el-01.html
(defun os/strip-whitespace ()
  "Untabify, strip white space, set file coding to UTF8."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun os/strip-whitespace-and-indent ()
  "Strip various whitespaces and reindent whole file."
  (interactive)
  (os/strip-whitespace)
  (indent-region (point-min) (point-max)))
(bind-key "C-c C-t" #'os/strip-whitespace-and-indent)


(provide 'misc-functions)
;;; misc-functions.el ends here
