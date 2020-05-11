;;; my-hooks.el --- my-hooks  -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs Configuration

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(add-hook 'emacs-lisp-mode (lambda () (add-hook 'after-save-hook 'check-parens nil t)))
(add-hook 'lisp-interaction-mode (lambda () (add-hook 'after-save-hook 'check-parens nil t)))
(add-hook 'lisp-mode (lambda () (add-hook 'after-save-hook 'check-parens nil t)))
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
;; (add-hook 'makefile-mode-map (lambda () (local-set-key (kbd "<f5>") 'compile)))
(add-hook 'minibuffer-exit-hook (lambda () (setq gc-cons-threshold 800000)))
(add-hook 'minibuffer-setup-hook (lambda () (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook '(lambda () (setq display-line-numbers 'relative)))
(add-hook 'sh-mode '(lambda () (setq display-line-numbers 'relative)))
(add-hook 'shell-script-mode '(lambda () (setq display-line-numbers 'relative)))
(add-hook 'text-mode-hook 'abbrev-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)

(provide 'my-hooks)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; my-hooks.el ends here
