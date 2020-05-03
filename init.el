;;; init.el --- init  -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs Configuration

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(let ((file-name-handler-alist nil)
      (gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.8))

  ;; setup package.el
  (require 'package)
  (setq package-enable-at-startup nil)
  (setq package--init-file-ensured t)
  (setq package-archives
        '(("melpa" . "https://melpa.org/packages/")
          ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  
  (byte-recompile-file (concat user-emacs-directory "keys.el") 'nil 0 t)
  (add-to-list 'load-path (concat user-emacs-directory "site-lisp/"))
  (byte-recompile-directory (concat user-emacs-directory "site-lisp") 0)
  (mapc (lambda (file) (load file))
        (directory-files (concat user-emacs-directory "site-lisp") t "\.elc$"))

  ;; install use-package
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  ;; `use-package' does all the heavy lifting in my config.
  (setq use-package-enable-imenu-support t)
  ;; (setq use-package-always-ensure t)
  (setq use-package-verbose t)
  (require 'use-package)
  (require 'bind-key)

  (byte-recompile-file (concat user-emacs-directory "init.el") 'nil 0 'nil)
  (byte-recompile-file (concat user-emacs-directory "use.el") 'nil 0 t)
  (message "Hacks and glory await! :-)"))

(provide 'init)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; init.el ends here
