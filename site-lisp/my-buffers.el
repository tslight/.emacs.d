;;; my-buffers.el --- my-buffers  -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs Configuration

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(defun my/fill-buffer ()
  "Fill the contents of a buffer."
  (interactive)
  (fill-region (point-min) (point-max)))

(defun my/indent-buffer ()
  "Indent the contents of a buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun my/kill-this-buffer ()
  "Kill the current buffer. `kill-this-buffer' is unreliable..."
  (interactive)
  (kill-buffer (current-buffer)))

(defun my/last-buffer ()
  "Switch back and forth between two buffers easily."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun my/nuke-buffers ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapc
   (lambda (buffer)
     (kill-buffer buffer))
   (buffer-list))
  (if current-prefix-arg
      (delete-other-windows)))

(defun my/save-buffers-silently ()
  "Save all open buffers without prompting."
  (interactive)
  (save-some-buffers t)
  (message "Saving all buffers..."))

(defun my/search-all-buffers (regexp)
  "Search all buffers for REGEXP."
  (interactive "sRegexp: ")
  (multi-occur-in-matching-buffers "." regexp t))

(defun my/toggle-buffer (buffer)
  "Toggle back & forth between BUFFER and the current buffer."
  (interactive "BBuffer: ")
  (if (equal (buffer-name) buffer)
      (my/last-buffer)
    (switch-to-buffer buffer)))

(defun my/toggle-maximize-buffer ()
  "Temporarily maximize a buffer."
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(my/bind-always "C-<escape>" my/last-buffer)
(my/bind-always "C-M-<escape>" my/toggle-buffer)
(my/bind-always "C-c TAB" my/indent-buffer)
(my/bind-always "C-c b i" my/indent-buffer)
(my/bind-always "C-c z" my/toggle-maximize-buffer)
(my/bind-always "C-c M-n" my/nuke-buffers)
(my/bind-always "C-c s" my/save-buffers-silently)
(my/bind-always "C-x k" my/kill-this-buffer)
(my/bind-always "M-s s" my/search-all-buffers)
(my/bind-always "C-c M-t m" (lambda () (interactive) (my/toggle-buffer "*Messages*")))
(my/bind-always "C-c M-t s" (lambda () (interactive) (my/toggle-buffer "*scratch*")))

(provide 'my-buffers)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; my-buffers.el ends here
