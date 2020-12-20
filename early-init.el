;;; early-init.el --- early-init  -*- lexical-binding: t; -*-

;;; Commentary:

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
;; Initialise installed packages
(byte-recompile-directory
 (expand-file-name "site-lisp" user-emacs-directory)
 0) ; directory needs to be first
(byte-recompile-file
 (expand-file-name "early-init.el" user-emacs-directory)
 'nil 0 'nil)
(byte-recompile-file
 (expand-file-name "init.el" user-emacs-directory)
 'nil 0 'nil)

;; This must be true otherwise use-package won't load!
(setq package-enable-at-startup t)
;; Allow loading from the package cache.
(setq package-quickstart t)
;; Don't write (package-initialize) to my init file!
(setq package--init-file-ensured t)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

(provide 'early-init)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; early-init.el ends here
