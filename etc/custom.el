;;; package --- Summary  ;; -*- lexical-binding: t; -*-

;;; Commentary:
;;;; Custom stuff.

;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-log-arguments '("--graph" "--color" "--decorate" "-n256"))
 '(package-selected-packages
   '(doom-modeline add-node-modules-path ag all-the-icons all-the-icons-completion all-the-icons-dired all-the-icons-ibuffer apheleia apib-mode browse-kill-ring cargo-mode conda consult corfu dash diminish disk editorconfig eglot-signature-eldoc-talkative emojify eslint-fix ess ess-R-data-view ess-r-insert-obj exec-path-from-shell expand-region fancy-compilation filladapt flymake flymake-collection flymake-css flymake-cursor flymake-diagnostic-at-point flymake-eslint flymake-jslint flymake-json flymake-markdownlint flymake-perlcritic flymake-python-pyflakes flymake-ruff flymake-sass flymake-shellcheck flymake-yamllint gcmh genehack-perl-elisp git-commit git-gutter handlebars-mode helpful html-to-markdown jq-format js2-mode json-mode json-navigator kind-icon kolon-mode magit magit-section marginalia markdown-mode move-text multi-term nodejs-repl nvm obsidian orderless perlcritic projectile python-black sass-mode smart-tab smartparens solarized-theme svelte-mode rustic terraform-mode treesit-auto tree-sitter-ess-r transient twilight-theme unicode-fonts vertico web-beautify web-mode which-key yaml-mode)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-error ((((class color)) (:underline "red"))))
 '(flymake-warning ((((class color)) (:underline "yellow")))))

(provide 'custom)
;;; custom.el ends here
