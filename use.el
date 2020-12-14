;;; use.el --- use-package initialisation -*- lexical-binding: t; -*-

;;; Commentary:

;; Use Package Startup

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
;; install use-package
;; `use-package' does all the heavy lifting in my config.
(require 'package)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-enable-imenu-support t)
;; (setq use-package-always-ensure t)
(setq use-package-verbose t)
(require 'use-package)

(byte-recompile-directory (concat user-emacs-directory "use.d") 0)
(byte-recompile-file (concat user-emacs-directory "use.el") 'nil 0 'nil)

;; load all files in ~/.emacs.d/use.d directory
(mapc (lambda (file) (load file))
      (directory-files (concat user-emacs-directory "use.d") t "^.*\.elc$"))
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; use.el ends here
