;;; hooks.el --- misc hooks -*- lexical-binding: t; -*-

;;; Commentary:

;; Miscellaneous hooks

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'lisp-mode-hook 'eldoc-mode)
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'shell-script-mode-hook 'hl-line-mode)
(add-hook 'text-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'yaml-mode-hook 'hs-minor-mode)
(add-hook 'yaml-mode-hook 'display-line-numbers-mode)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; hooks.el ends here
