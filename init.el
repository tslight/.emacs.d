;;; init.el --- init  -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs Configuration

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(let ((file-name-handler-alist nil)
      (gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.8))

  (mapc (lambda (file) (load file))
        (directory-files (concat user-emacs-directory "site-lisp") t "^init\-.*\.elc$"))

  ;; install use-package
  (require 'package)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  ;; `use-package' does all the heavy lifting in my config.
  (setq use-package-enable-imenu-support t)
  ;; (setq use-package-always-ensure t)
  (setq use-package-verbose t)
  (require 'use-package)

  ;; add my custom libraries to load path and compile them & the init file
  (add-to-list 'load-path (concat user-emacs-directory "site-lisp/"))
  (byte-recompile-directory (concat user-emacs-directory "site-lisp") 0)
  (byte-recompile-file (concat user-emacs-directory "init.el") 'nil 0 'nil)

  ;; load all files in site-lisp that start with the `use' prefix.
  (mapc (lambda (file) (load file))
        (directory-files (concat user-emacs-directory "site-lisp") t "^use\-.*\.elc$"))

  (message "HACKS AND GLORY AWAIT! :-)"))
(provide 'init)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; init.el ends here
