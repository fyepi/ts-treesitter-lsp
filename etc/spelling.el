;;; spelling.el -- Customizations spelling software.  ;; -*- lexical-binding: t; -*-

;;; Commentary:
;;;; Spelling config optimized for programmers

;;; Code:


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

(use-package wucuo
  :straight t
  :hook ((prog-mode text-mode) wucuo-start))

(provide 'spelling)
;;; spelling.el ends here
