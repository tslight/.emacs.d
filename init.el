;;; init.el --- init  -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs Configuration

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(let ((file-name-handler-alist nil)
      (gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.8))

  ;; add my custom libraries to load path and compile them & the init file
  (byte-recompile-directory (concat user-emacs-directory "early-init.d") 0)
  (byte-recompile-directory (concat user-emacs-directory "init.d") 0)
  (byte-recompile-directory (concat user-emacs-directory "site-lisp") 0)
  (byte-recompile-file (concat user-emacs-directory "early-init.el") 'nil 0 'nil)
  (byte-recompile-file (concat user-emacs-directory "init.el") 'nil 0 'nil)

  ;; load all files in ~/.emacs.d/init.d
  (mapc (lambda (file) (load file))
        (directory-files (concat user-emacs-directory "init.d") t "^.*\.elc$"))

  ;; install use-package
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (byte-recompile-directory (concat user-emacs-directory "use.d") 0)
  (byte-recompile-file (concat user-emacs-directory "use.el") 'nil 0 'nil)

  (add-to-list 'load-path (concat user-emacs-directory "site-lisp/"))
  (load (concat user-emacs-directory "use.elc"))

  (message "HACKS AND GLORY AWAIT! :-)"))
(provide 'init)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; init.el ends here
