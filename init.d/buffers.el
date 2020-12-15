;;; buffers.el --- buffers  -*- lexical-binding: t; -*-

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
  "Kill the current buffer - `kill-this-buffer' is unreliable."
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

(defun my/toggle-messages ()
  "Toggle *Messages* buffer."
  (interactive)
  (my/toggle-buffer "*Messages*"))

(defun my/toggle-scratch ()
  "Togggle *scratch* buffer."
  (interactive)
  (my/toggle-buffer "*scratch*"))

(defun my/toggle-maximize-buffer ()
  "Temporarily maximize a buffer."
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(global-set-key (kbd "C-c b b") 'my/last-buffer)
(global-set-key (kbd "C-c b t") 'my/toggle-buffer)
(global-set-key (kbd "C-<escape>") 'my/last-buffer)
(global-set-key (kbd "C-M-<escape>") 'my/toggle-buffer)
(global-set-key (kbd "C-c TAB") 'my/indent-buffer)
(global-set-key (kbd "C-c b i") 'my/indent-buffer)
(global-set-key (kbd "C-c z") 'my/toggle-maximize-buffer)
(global-set-key (kbd "C-c M-n") 'my/nuke-buffers)
(global-set-key (kbd "C-c s") 'my/save-buffers-silently)
(global-set-key (kbd "C-x k") 'my/kill-this-buffer)
(global-set-key (kbd "M-s s") 'my/search-all-buffers)
(global-set-key (kbd "C-c M-t m") 'my/toggle-messages)
(global-set-key (kbd "C-c M-t s") 'my/toggle-scratch)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; buffers.el ends here
