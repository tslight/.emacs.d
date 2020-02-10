;;; my-tangles.el ---

;;; Commentary:

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight tslight@pm.me

;;; Code:
;; -*- lexical-binding: t; -*-
(defgroup my/configuration-directories nil
  "Quick manipulation of textual checkboxes."
  :group 'convenience)
(defcustom my/config-directory "~/src/tspub/devops/etc/"
  "Path to parent configuration directory."
  :group 'my/configuration-directories
  :type 'directory)
(defcustom my/agnostic-config-directory (concat my/config-directory "agnostic/")
  "Path to agnostic configuration directory."
  :group 'my/configuration-directories
  :type 'directory)
(defcustom my/emacs-config-directory (concat my/config-directory "emacs/")
  "Path to emacs configuration directory."
  :group 'my/configuration-directories
  :type 'directory)

(defun my/os-config-directory ()
  "Finds the directory in `my/config-directory' that matches a
substring of `system-configuration'."
  (car (remove nil
	       (mapcar (lambda (d)
			 (if (and (file-directory-p d)
				  (string-match
				   (file-name-base d) system-configuration))
			     d))
		       (directory-files my/config-directory t "^[^\\..]*$")))))

(defun my/insert-boilerplate (&optional file)
  "Take a FILE as an argument & insert elisp header and footer
comments into it, if it's an elisp FILE.

If the file is in my conf directory also insert a provide
expression, so that the FILE can be loaded as a package.

Return the file name, so that this function can be piped to other
functions, such as `my/compile-and-load-file'."
  (when file
    (when (equal (file-name-extension file) "el")
      (let ((temp-buffer (set-buffer (find-file-noselect file))))
	(when temp-buffer
	  (unwind-protect
	      (progn
		(let* ((filename (file-name-sans-extension
				  (file-name-nondirectory file)))
		       (copyright (concat
				   (format-time-string "%Y")
				   " "
				   user-full-name))
		       (author (concat user-full-name " " user-mail-address))
		       (header (concat ";;; " filename ".el ---\n\n"
				       ";;; Commentary:\n\n"
				       ";; Copyright (C) " copyright "\n"
				       ";; Author: " author "\n\n"
				       ";;; Code:\n"
				       ";; -*- lexical-binding: t; -*-\n"))
		       (footer (concat "\n(provide '" filename ")\n"
				       ";;; " filename ".el ends here")))
		  (goto-char (point-min))
		  (insert header)
		  (goto-char (point-max))
		  (insert footer))
		(message (concat "Inserted boilerplate into " file)))
	    (save-buffer temp-buffer)
	    (kill-buffer temp-buffer))))
      (message (concat "Cleaning up " file"~ backup file..."))
      (when (file-readable-p (concat file "~"))
	(delete-file (concat file "~"))
	(message (concat "Deleted " file "~ backup file"))))
    file))

(defun my/compile-and-load-config ()
  "Byte compile init file and conf directory and then load
   the newly compiled configuration."
  (interactive)
  (if (not (byte-compile-file (concat user-emacs-directory "init.el") 0))
      (load-file (concat user-emacs-directory "init.el"))))

(defun my/tangle-directory (directory)
  "Takes a DIRECTORY as an argument and recursively tangles all
  the org-mode files in it and it's child directories.

  If the directory being tangled is my emacs configuration
  directory, run my/compile-and-load-config."
  (interactive "DDirectory: ")
  (unless (featurep 'org) (require 'org)) ; needed for bootstrap

  (mapc (lambda (file)
	  (my/insert-boilerplate
	   (format "%s" (car (org-babel-tangle-file file)))))
	(directory-files-recursively directory ".*\\.org$"))
  (when (string-prefix-p
	 (replace-regexp-in-string "/$" ""
				   (file-truename my/emacs-config-directory))
	 (replace-regexp-in-string "/$" ""
				   (file-truename directory)))
    (my/compile-and-load-config)
    (mapc (lambda (file)
	    (if (not (file-directory-p file))
		(delete-file file)))
	  (directory-files (getenv "HOME") t "^\\.emacs.*")))

  (message (concat "my/tangle-directory "directory" completed.")))

(defun my/tangle-all ()
  "Tangle all the things!"
  (interactive)
  (my/tangle-directory my/agnostic-config-directory)
  (my/tangle-directory my/emacs-config-directory)
  (my/tangle-directory (my/os-config-directory))
  (message "Tangled all the things!"))

(defun my/auto-tangle ()
  "Auto tangle files in the directories specified below when they
are edited. Providing the system-type value is

It doesn't make sense to tangle openbsd configurations files on
to a linux box, for example.

However, I may want to be able to edit them without auto tangling
from another OS."
  ;; first check we are not editing something in the .git metadata directory
  ;; (ie COMMIT_EDITMSG - we don't want to try and compile and load that!)
  (if (not (string-match-p "/.git/" buffer-file-name))
      (let ((current-directory-path
	     (file-truename
	      (file-name-directory
	       (directory-file-name buffer-file-name)))))
	(cond ((string-prefix-p (expand-file-name my/emacs-config-directory)
				current-directory-path)
	       (byte-compile-file
		(my/insert-boilerplate
		 (expand-file-name (car (org-babel-tangle))))
		0))
	      ((or (string-prefix-p (expand-file-name my/agnostic-config-directory)
				    current-directory-path)
		   (string-prefix-p (expand-file-name (my/os-config-directory))
				    current-directory-path))
	       (org-babel-tangle))))))

(defun my/hardware-tangle (vendor file path)
  "Tangle block to PATH if VENDOR string matches the relevant FILE in
    /sys/class/dmi/id/."
  (if (string-match vendor
		    (with-temp-buffer
		      (insert-file-contents (concat "/sys/class/dmi/id/" file))
		      (buffer-string)))
      path
    "no"))

(defun my/os-tangle (os)
  (when (not (equal system-type os)) "no"))

(global-set-key (kbd "C-c v d") 'my/tangle-directory)
(global-set-key (kbd "C-c v a") 'my/tangle-all)

(add-hook 'after-save-hook 'my/auto-tangle)

(provide 'my-tangles)
;;; my-tangles.el ends here
