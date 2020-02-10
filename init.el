;;; init.el ---

;;; Commentary:

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight tslight@pm.me

;;; Code:
;; -*- lexical-binding: t; -*-
(let ((file-name-handler-alist nil)
      (gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.8))

  (add-to-list 'load-path (concat user-emacs-directory "site-lisp/"))
  (byte-recompile-directory (concat user-emacs-directory "site-lisp") 0)
  (mapc (lambda (file) (load file))
	(directory-files (concat user-emacs-directory "site-lisp") t "\.elc$"))

  ;; setup package.el
  (require 'package)
  (setq package-enable-at-startup nil)
  (setq package--init-file-ensured t)
  (setq package-archives
	'(("melpa" . "https://melpa.org/packages/")
	  ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)

  ;; install use-package
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  ;; `use-package' does all the heavy lifting in my config.
  (setq use-package-enable-imenu-support t)
  ;; (setq use-package-always-ensure t)
  (setq use-package-verbose t)
  (eval-when-compile
    (require 'use-package))
  (require 'bind-key)

  (byte-recompile-file (concat user-emacs-directory "init.el") 'nil 0 'nil)
  (byte-recompile-file (concat user-emacs-directory "use.el") 'nil 0 t)
  (message "Hacks and glory await! :-)"))

(provide 'init)
;;; init.el ends here
