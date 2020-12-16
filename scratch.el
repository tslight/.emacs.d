;;; scratch.el --- elisp playground -*- lexical-binding: t; -*-

;;; Commentary:

;; Just random crap

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(locate-library "dired")
(autoload 'ibuffer "ibuffer" nil t)
(autoload 'dired "dired" nil t)
(autoloadp 'dired)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))
(locate-library "eshell")
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; scratch.el ends here
