;;; my-files.el --- file wrangling functions  -*- lexical-binding: t; -*-

;;; Commentary:

;; file wrangling functions

;; Emacs Configuration

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(require 'dired)

(defun my/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun my/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun my/make-backup ()
  "Make a backup copy of current file or dired marked files.

If in dired, backup current file or marked files."
  (interactive)
  (let (($fname (buffer-file-name)))
    (if $fname
        (let (($backup-name
               (concat $fname "." (format-time-string "%y%m%d%H%M") ".bak")))
          (copy-file $fname $backup-name t)
          (message (concat "Backup saved at: " $backup-name)))
      (if (string-equal major-mode "dired-mode")
          (progn
            (mapc (lambda ($x)
                    (let (($backup-name
                           (concat $x "." (format-time-string "%y%m%d%H%M") ".bak")))
                      (copy-file $x $backup-name t)))
                  (dired-get-marked-files))
            (message "marked files backed up"))
        (user-error "Buffer not file nor dired")))))

(defun my/make-backup-and-save ()
  "Backup of current file and save, or backup dired marked files.
For detail, see `my/make-backup'."
  (interactive)
  (if (buffer-file-name)
      (progn
        (my/make-backup)
        (when (buffer-modified-p)
          (save-buffer)))
    (progn
      (my/make-backup))))

(defun my/rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "FNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (when (file-exists-p filename)
          (rename-file filename new-name 1))
        (rename-buffer new-name)
        (set-visited-file-name new-name)))))

(defun my/read-file (file)
  "Return FILE content as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun my/read-lines (file)
  "Return a list of lines of a FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defun my/sudoedit (&optional arg)
  "Open current or ARG file as root."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (read-file-name "Find file (as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(provide 'my-files)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; my-files.el ends here
