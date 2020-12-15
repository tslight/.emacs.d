;;; early-init.el --- early-init  -*- lexical-binding: t; -*-

;;; Commentary:

;; Early Emacs Configuration

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
;; load all files ~/.emacs.d/early-init.d
(byte-recompile-file (concat user-emacs-directory "early-init.el") 'nil 0 'nil)
;; Initialise installed packages
(setq package-enable-at-startup t)
;; Allow loading from the package cache.
(setq package-quickstart t)
(setq package--init-file-ensured t)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; early-init.el ends here
