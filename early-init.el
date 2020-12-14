;;; early-init.el --- early-init  -*- lexical-binding: t; -*-

;;; Commentary:

;; Early Emacs Configuration

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
;; Initialise installed packages
;; load all files in site-lisp that start with the `init' prefix.
(mapc (lambda (file) (load file))
      (directory-files (concat user-emacs-directory "site-lisp") t "^init\-.*\.elc$"))

(setq package-enable-at-startup t)
;; Allow loading from the package cache.
(setq package-quickstart t)
(setq package--init-file-ensured t)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
;; Do not resize the frame at this early stage.
;; (setq frame-inhibit-implied-resize t)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; early-init.el ends here
