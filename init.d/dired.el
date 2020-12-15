;;; dired.el --- dired configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; Dired Configuration

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
;; This is only needed once, near the top of the file
(with-eval-after-load 'dired
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

  (autoload 'find-ls-option "find-dired")

  (autoload 'dired-omit-mode "dired-x"
    "Omit files from dired listings." t)

  (autoload 'dired-omit-files "dired-x"
    "User regex to specify what files to omit." t)

  (autoload 'dired-jump "dired-x"
    "Jump to Dired buffer corresponding to current buffer." t)

  (autoload 'dired-jump-other-window "dired-x"
    "Like \\[dired-jump] (dired-jump) but in other window." t)

  (when (eq system-type 'berkeley-unix)
    (progn
      (setq dired-listing-switches "-alhpL")))

  (setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))
  (setq dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'\\|^\\..+$")
  (setq dired-dwim-target t
        dired-use-ls-dired nil
        dired-recursive-copies 'always
        dired-recursive-deletes 'always)

  (global-set-key (kbd "C-x C-d") 'dired)
  (global-set-key (kbd "C-x C-j") 'dired-jump)
  (global-set-key (kbd "C-x 4 C-j") 'dired-jump)
  (global-set-key (kbd "C-x d") 'dired-jump)
  (global-set-key (kbd "C-x 4 d") 'dired-jump)
  (global-set-key (kbd "C-x M-d") 'list-directory)

  (define-key dired-mode-map "b" (lambda () (interactive (find-alternate-file ".."))))
  (define-key dired-mode-map "f" 'dired-find-alternate-file)
  (define-key dired-mode-map "c" 'dired-do-compress-to)
  (define-key dired-mode-map ")" 'dired-omit-mode)
  (define-key dired-mode-map (kbd "C-o") 'dired-find-file-other-window)

  (define-key dired-mode-map "o" 'my/dired-view-current)
  (define-key dired-mode-map "s" 'my/dired-sort)
  (define-key dired-mode-map "?" 'my/dired-get-size)
  (define-key dired-mode-map (kbd "C-RET") 'my/dired-open-marked-files)
  (define-key dired-mode-map (vector 'remap 'end-of-buffer) 'my/dired-jump-to-bottom)
  (define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'my/dired-back-to-top)

  (message "Successfully loaded `dired' :-)"))
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; dired.el ends here
