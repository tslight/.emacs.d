;;; init.el --- init  -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs Configuration

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(let ((file-name-handler-alist nil)
      (gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.8))

  (add-to-list 'load-path (concat user-emacs-directory "site-lisp/"))

  (mapc (lambda (directory) (byte-recompile-directory (concat user-emacs-directory directory) 0))
        '("early-init.d" "init.d" "site-lisp")) ;; compile these directories

  (mapc (lambda (file) (byte-recompile-file (concat user-emacs-directory file) 'nil 0 'nil))
        '("early-init.el" "init.el")) ;; compile these files

  (mapc (lambda (file) (load file)) ;; load all files in ~/.emacs.d/init.d
        (directory-files (concat user-emacs-directory "init.d") t "^.*\.elc$"))

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (byte-recompile-directory (concat user-emacs-directory "use.d") 0)
  (byte-recompile-file (concat user-emacs-directory "use.el") 'nil 0 'nil)

  (load (concat user-emacs-directory "use")) ;; comment for no 3rd party packages

  (message "HACKS AND GLORY AWAIT! :-)"))
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; init.el ends here
