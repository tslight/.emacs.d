;;; early-init.el --- early-init  -*- lexical-binding: t; -*-

;;; Commentary:

;; Early Emacs Configuration

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
;; load all files ~/.emacs.d/early-init.d
(byte-recompile-directory (concat user-emacs-directory "early-init.d") 0)
(byte-recompile-file (concat user-emacs-directory "early-init.el") 'nil 0 'nil)

(mapc (lambda (file) (load file))
      (directory-files
       (concat user-emacs-directory "early-init.d") t "^.*\.elc$"))
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; early-init.el ends here
