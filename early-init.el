;;; early-init.el --- early-init  -*- lexical-binding: t; -*-

;;; Commentary:

;; Early Emacs Configuration

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
;; Initialise installed packages
(byte-recompile-directory (concat user-emacs-directory "site-lisp") 0) ; directory needs to be first
(byte-recompile-file (concat user-emacs-directory "early-init.el") 'nil 0 'nil)
(byte-recompile-file (concat user-emacs-directory "init.el") 'nil 0 'nil)

;; This must be true otherwise use-package won't load!
(setq package-enable-at-startup t)
;; Allow loading from the package cache.
(setq package-quickstart t)
;; Don't write (package-initialize) to my init file!
(setq package--init-file-ensured t)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; early-init.el ends here
