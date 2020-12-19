;;; init.el --- init  -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs Configuration

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(let ((file-name-handler-alist nil)
      (gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.8))

  (mapc (lambda (file)
          (load file)) ;; Load all files in ~/.emacs.d/site-lisp
        (directory-files ;; Get all *.elc files in ~/.emacs/site-lisp
         (expand-file-name "site-lisp" user-emacs-directory)
         t "^.*\.elc$"))

  (load (expand-file-name "use" user-emacs-directory)))
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; init.el ends here
