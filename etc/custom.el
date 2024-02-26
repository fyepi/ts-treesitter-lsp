;;; package --- Summary  ;; -*- lexical-binding: t; -*-

;;; Commentary:
;;;; Custom stuff.

;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" default))
 '(magit-log-arguments '("--graph" "--color" "--decorate" "-n256"))
 '(package-selected-packages
   '(consult-dir embark-consult embark vertico-directory vterm-toggle doom-themes doom-theme cider clojure-mode-extra-font-locking clojure-mode doom-modeline add-node-modules-path ag all-the-icons all-the-icons-completion all-the-icons-dired all-the-icons-ibuffer apheleia apib-mode browse-kill-ring cargo-mode conda consult corfu dash diminish disk editorconfig eglot-signature-eldoc-talkative emojify eslint-fix ess ess-R-data-view ess-r-insert-obj exec-path-from-shell expand-region fancy-compilation filladapt flymake flymake-collection flymake-css flymake-cursor flymake-diagnostic-at-point flymake-eslint flymake-jslint flymake-json flymake-markdownlint flymake-perlcritic flymake-python-pyflakes flymake-ruff flymake-sass flymake-shellcheck flymake-yamllint gcmh genehack-perl-elisp git-commit git-gutter handlebars-mode helpful html-to-markdown jq-format js2-mode json-mode json-navigator kind-icon kolon-mode magit magit-section marginalia markdown-mode move-text multi-term nodejs-repl nvm obsidian orderless perlcritic projectile python-black sass-mode smart-tab smartparens solarized-theme svelte-mode rustic terraform-mode treesit-auto tree-sitter-ess-r transient twilight-theme unicode-fonts vertico web-beautify web-mode which-key yaml-mode)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-error ((((class color)) (:underline "red"))))
 '(flymake-warning ((((class color)) (:underline "yellow"))))
 '(line-number ((t (:inherit default :background "#002b36" :foreground "#586e75" :underline nil :slant italic :weight light))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))

(provide 'custom)
;;; custom.el ends here
