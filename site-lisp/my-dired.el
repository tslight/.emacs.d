;;; my-dired.el --- my-dired  -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs Configuration

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(require 'dired)

(defun my/dired-get-size ()
  "Get cumlative size of marked or current item."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message "Size of all marked files: %s"
               (progn
                 (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
                 (match-string 1))))))

(defun my/dired-open-marked-files ()
  "Open marked files."
  (interactive)
  (let ((distinguish-one-marked nil))
    (mapc 'find-file
          (dired-map-over-marks
           (dired-get-file-for-visit)
           current-prefix-arg))))

(defun my/dired-sort ()
  "Sort dired dir listing in different ways.  Prompt for a choice."
  (interactive)
  (let (-sort-by -arg)
    (if (eq system-type 'berkeley-unix)
        (progn
          (setq -sort-by (completing-read
                          "Sort by:" '( "date" "size" "name")))
          (cond ((equal -sort-by "name") (setq -arg "-alhpL"))
                ((equal -sort-by "date") (setq -arg "-alhpLt"))
                ((equal -sort-by "size") (setq -arg "-alhpLS "))
                (t (error "Logic error 09535" ))))
      (progn
        (setq -sort-by (completing-read
                        "Sort by:" '( "date" "size" "name" "dir")))
        (cond ((equal -sort-by "name")
               (setq -arg "-Al --si --time-style long-iso "))
              ((equal -sort-by "date")
               (setq -arg "-Al --si --time-style long-iso -t"))
              ((equal -sort-by "size")
               (setq -arg "-Al --si --time-style long-iso -S"))
              ((equal -sort-by "dir")
               (setq -arg "-Al --si --time-style long-iso --group-directories-first"))
              (t (error "Logic error 09535" )))))
    (dired-sort-other -arg )))

(defun my/dired-back-to-top ()
  "Go to first file in directory."
  (interactive)
  (goto-char (point-min))
  (dired-next-line 2))

(defun my/dired-jump-to-bottom ()
  "Go to last file in directory."
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1))

(defun my/dired-view-current ()
  "View current file in read-only temporary buffer and other window."
  (interactive)
  (if (not (window-parent))
      (split-window-horizontally))
  (let ((file (dired-get-file-for-visit))
        (dbuffer (current-buffer)))
    (other-window 1)
    (unless (equal dbuffer (current-buffer))
      (if (or view-mode (equal major-mode 'dired-mode))
          (kill-buffer)))
    (let ((filebuffer (get-file-buffer file)))
      (if filebuffer
          (switch-to-buffer filebuffer)
        (view-file file))
      (other-window -1))))

(defvar dired-compress-files-alist
  '(("\\.tar\\.gz\\'" . "tar -c %i | gzip -c9 > %o")
    ("\\.zip\\'" . "zip %o -r --filesync %i"))
  "Control the compression shell command for `dired-do-compress-to'.

Each element is (REGEXP . CMD), where REGEXP is the name of the
archive to which you want to compress, and CMD the the
corresponding command.

Within CMD, %i denotes the input file(s), and %o denotes the
output file.  %i path(s) are relative, while %o is absolute.")

(provide 'my-dired)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; my-dired.el ends here
