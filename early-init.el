;;; early-init.el --- early-init  -*- lexical-binding: t; -*-

;;; Commentary:

;; Early Emacs Configuration

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
;; load all files ~/.emacs.d/early-init.d
(mapc (lambda (file) (load file))
      (directory-files
       (concat user-emacs-directory "early-init.d") t "^.*\.elc$"))
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; early-init.el ends here
