;;; init.el --- init  -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs Configuration

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(let ((file-name-handler-alist nil)
      (gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.8))

  (mapc (lambda (directory) (byte-recompile-directory (concat user-emacs-directory directory) 0))
        '("site-lisp")) ;; compile these directories under ~/.emacs.d

  (mapc (lambda (file) (byte-recompile-file (concat user-emacs-directory file) 'nil 0 'nil))
        '("init.el")) ;; compile these files in ~/.emacs.d

  (mapc (lambda (file) (load file)) ;; load all files in ~/.emacs.d/site-lisp
        (directory-files (concat user-emacs-directory "site-lisp") t "^.*\.elc$"))

  (load (concat user-emacs-directory "use")) ;; comment for no 3rd party packages

  (message "Hacks and glory await! :-)"))
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; init.el ends here
