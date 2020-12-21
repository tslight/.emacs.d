;;; early-init.el --- early-init  -*- lexical-binding: t; -*-

;;; Commentary:

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'tooltip-mode) (tooltip-mode -1))

(setq frame-resize-pixelwise t) ;; jwm resize fix

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
