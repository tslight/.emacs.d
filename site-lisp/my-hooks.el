;;; my-hooks.el --- my-hooks  -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs Configuration

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(add-hook 'emacs-lisp-mode 'eldoc-mode)
(add-hook 'emacs-lisp-mode (lambda () (add-hook 'after-save-hook 'check-parens nil t)))

(add-hook 'lisp-interaction-mode 'eldoc-mode)
(add-hook 'lisp-interaction-mode (lambda () (add-hook 'after-save-hook 'check-parens nil t)))

(add-hook 'lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-mode (lambda () (add-hook 'after-save-hook 'check-parens nil t)))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

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

(setq indent-tabs-mode 'nil) ;; don't undo untabify
(setq whitespace-style '(face
                         tabs
                         spaces
                         trailing
                         lines
                         space-before-tab::space
                         newline
                         indentation::space
                         empty
                         space-after-tab::space
                         space-mark
                         tab-mark
                         newline-mark))
(add-hook 'before-save-hook 'whitespace-cleanup)

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
;; https://stackoverflow.com/a/7437783
(add-hook 'term-exec-hook '(lambda () (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)))

(provide 'my-hooks)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; my-hooks.el ends here
