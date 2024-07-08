;;; corgi-clojure.el --- Clojure configuration for Corgi -*- lexical-binding: t -*-
;;; Commentary:
;;; Taken from https://github.com/corgi-emacs/corgi-packages/blob/main/corgi-clojure/corgi-clojure.el
;;
;; Filename: corgi-clojure.el
;; Package-Requires: ((use-package) (cider) (clj-ns-name) (clojure-mode))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(use-package clojure-mode
  :straight '(clojure-mode :type git :host github :repo "clojure-emacs/clojure-mode")
  :magic ("^#![^\n]*/\\(clj\\|clojure\\|bb\\|lumo\\)" . clojure-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.ednl$" . clojure-mode))
  :config
  (setq clojure-toplevel-inside-comment-form t
        ;; Because of CIDER's insistence to send forms to all linked REPLs, we
        ;; *have* to be able to switch cljc buffer to clj/cljs mode without
        ;; cider complaining.
        clojure-verify-major-mode nil))

(use-package cider
  :straight '(cider :type git :host github :repo "clojure-emacs/cider")
  :diminish cider-mode
  :bind (:map cider-mode-map
              ("M-¬" . cider-format-buffer))
  :config
  (setq cider-preferred-build-tool 'clojure-cli
        ;; ~make sure we can always debug nrepl issues~
        ;; Turning this off again, seems it may really blow up memory usage
        nrepl-log-messages nil))

(provide 'clojure)
;;; clojure.el ends here
