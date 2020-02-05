;;; init.el --- Emacs Initialisation File

;;; Commentary:

;; Copyright (C) 2019
;; Author:  Toby Slight

;;; Code:
;; -*- lexical-binding: t; -*-
(let ((file-name-handler-alist nil)
      (gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.8))
  (add-to-list 'load-path (concat user-emacs-directory "site-lisp/"))
  (byte-recompile-directory (concat user-emacs-directory "site-lisp") 0)
  (byte-recompile-file (concat user-emacs-directory "init.el") 'nil 0 'nil)
  (message "Hacks and glory await! :-)"))

(provide 'init)
;;; init.el ends here
