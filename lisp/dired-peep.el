;;; dired-peep.el --- peep files in dired -*- lexical-binding: t; -*-

;;; Commentary:

;; Temporarily view files when navigating dired buffers.

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
;; Show buffer content when exploring a directory with Dired.
;; Press "n"/p to show the next/previous buffer in other window. Useful to
;; have a glimpse of files when exploring a directory.
;; It is possible to browse without file previews with the arrow keys.

(defgroup dired-peep nil
  "See the file at point when browsing in a Dired buffer."
  :group 'dired)

(setq dired-peep-next-current-buffer nil)

;;;###autoload
(defun dired-peep-next ()
  (interactive)
  (next-line 1)
  (dired-find-file-other-window)
  (if dired-peep-next-current-buffer (kill-buffer dired-peep-next-current-buffer))
  (setq dired-peep-next-current-buffer (current-buffer))
  (other-window 1))

;;;###autoload
(defun dired-peep-previous ()
  (interactive)
  (previous-line 1)
  (dired-find-file-other-window)
  (if dired-peep-next-current-buffer (kill-buffer dired-peep-next-current-buffer))
  (setq dired-peep-next-current-buffer (current-buffer))
  (other-window 1))

;;;###autoload
(define-minor-mode dired-peep-mode
  "Toggle preview of files when browsing in a Dired buffer."
  :global t
  :group 'dired-peep
  (if dired-peep-mode
      (progn
        (define-key dired-mode-map "n" 'dired-peep-next)
        (define-key dired-mode-map "p" 'dired-peep-previous))
    (define-key dired-mode-map "n" 'dired-next-line)
    (define-key dired-mode-map "p" 'dired-previous-line)))

(provide 'dired-peep)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; dired-peep.el ends here
