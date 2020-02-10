;;; my-hooks.el ---

;;; Commentary:

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight tslight@pm.me

;;; Code:
;; -*- lexical-binding: t; -*-
(add-hook 'before-save-hook 'whitespace-cleanup)

(add-hook 'emacs-lisp-mode 'eldoc-mode)
(add-hook 'emacs-lisp-mode (lambda () (add-hook 'after-save-hook 'check-parens nil t)))

(add-hook 'lisp-interaction-mode 'eldoc-mode)
(add-hook 'lisp-interaction-mode (lambda () (add-hook 'after-save-hook 'check-parens nil t)))

(add-hook 'lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-mode (lambda () (add-hook 'after-save-hook 'check-parens nil t)))

;; (add-hook 'makefile-mode-map (lambda () (local-set-key (kbd "<f5>") 'compile)))

(add-hook 'minibuffer-exit-hook (lambda () (setq gc-cons-threshold 800000)))
(add-hook 'minibuffer-setup-hook (lambda () (setq gc-cons-threshold most-positive-fixnum)))

(add-hook 'prog-mode-hook 'electric-indent-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook '(lambda () (setq display-line-numbers 'relative)))

(add-hook 'sh-mode 'electric-indent-mode)
(add-hook 'sh-mode 'electric-pair-mode)
(add-hook 'sh-mode 'hl-line-mode)
(add-hook 'sh-mode '(lambda () (setq display-line-numbers 'relative)))
(add-hook 'shell-script-mode 'electric-indent-mode)
(add-hook 'shell-script-mode 'electric-pair-mode)
(add-hook 'shell-script-mode 'hl-line-mode)
(add-hook 'shell-script-mode '(lambda () (setq display-line-numbers 'relative)))

(add-hook 'text-mode 'hl-line-mode)
(add-hook 'text-mode-hook 'abbrev-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)

(provide 'my-hooks)
;;; my-hooks.el ends here
