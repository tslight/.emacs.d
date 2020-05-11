;;; init.el --- init  -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs Configuration

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(let ((file-name-handler-alist nil)
      (gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.8))

  (byte-recompile-file (concat user-emacs-directory "keys.el") 'nil 0 t)
  (add-to-list 'load-path (concat user-emacs-directory "site-lisp/"))
  (byte-recompile-directory (concat user-emacs-directory "site-lisp") 0)
  (mapc (lambda (file) (load file))
        (directory-files (concat user-emacs-directory "site-lisp") t "\.elc$"))

  (byte-recompile-file (concat user-emacs-directory "init.el") 'nil 0 'nil)
  (message "Hacks and glory await! :-)"))

(provide 'init)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; init.el ends here
