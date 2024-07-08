;;; dired-config.el Dired config
;;; Commentary:
;;; Code:
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
  :commands
  dired
  dired-jump
  dired-next-line
  dired-up-directory
  :bind
  (:map dired-mode-map
        ("-"                         . dired-up-directory)
        ("E"                         . wdired-change-to-wdired-mode)
        ([remap beginning-of-buffer] . dired-back-to-top)
        ([remap end-of-buffer]       . dired-jump-to-bottom)))

;; Dired extra font locking
(use-package diredfl
  :ensure t
  :hook (dired-mode .diredfl-mode))

(provide 'dired-config)
;;; dired.el ends here
