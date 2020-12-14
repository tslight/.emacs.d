;;; init-keys.el --- custom key bindings -*- lexical-binding: t; -*-

;;; Commentary:

;; Custom key bindings

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(global-set-key (kbd "C-c ;") 'comment-line)
(global-set-key (kbd "C-z") 'zap-up-to-char)
(global-set-key (kbd "M-z") 'zap-up-char)
(global-set-key (kbd "C-x M-t") 'transpose-regions)
(global-set-key (kbd "C-x M-p") 'transpose-paragraphs)
(global-set-key (kbd "C-c e b") 'eval-buffer)
(global-set-key (kbd "C-c e e") 'pp-eval-last-sexp)
(global-set-key (kbd "C-c e m") 'lisp-interaction-mode)
(global-set-key (kbd "C-c M-t a") 'toggle-text-mode-autofill)
(global-set-key (kbd "C-c M-t d E") 'toggle-debug-on-entry)
(global-set-key (kbd "C-c M-t d e") 'toggle-debug-on-error)
(global-set-key (kbd "C-c M-t d q") 'toggle-debug-on-quit)
(global-set-key (kbd "C-c M-t t") 'toggle-truncate-lines)
(global-set-key (kbd "C-c M-t m") 'menu-bar-mode)
(global-set-key (kbd "C-c h n") 'highlight-changes-next-change)
(global-set-key (kbd "C-c h p") 'highlight-changes-previous-change)
(global-set-key (kbd "C-x c") 'save-buffers-kill-emacs)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x M-k") 'kill-buffer)
(global-set-key (kbd "C-x t t") 'tab-bar-select-tab-by-name)
(global-set-key (kbd "C-x t c") 'tab-bar-new-tab)
(global-set-key (kbd "C-x t k") 'tab-bar-close-tab)
(global-set-key (kbd "C-x t n") 'tab-bar-switch-to-next-tab)
(global-set-key (kbd "C-x t p") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "C-x t l") 'tab-bar-switch-to-recent-tab)
(global-set-key (kbd "C-<f10>") 'toggle-frame-maximized)
(global-set-key (kbd "C-<f11>") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-s-m") 'toggle-frame-maximized)
(global-set-key (kbd "C-c C-r") 'recentf-open-files)
(global-set-key (kbd "C-c M-d r") 'desktop-read)
(global-set-key (kbd "C-c M-d s") 'desktop-save)
(global-set-key (kbd "S-<f10>") 'menu-bar-mode)
(global-set-key (kbd "C-c v") 'scroll-other-window-down)
(global-set-key (kbd "C-c C-/") 'winner-undo)
(global-set-key (kbd "C-c C-?") 'winner-redo)
(global-set-key (kbd "C-c M-c") 'calculator)
(global-set-key (kbd "C-c c") 'calc)
(global-set-key (kbd "C-x j") 'jump-to-register)
(global-set-key (kbd "C-x p") 'pop-to-mark-command)
(global-set-key (kbd "M-SPC") 'cycle-spacing)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-r") 'multi-isearch-buffers-regexp)
(global-set-key (kbd "C-M-s") 'multi-isearch-files-regexp)
;; for help modes, and simple/special modes
(define-key special-mode-map "n" #'forward-button)
(define-key special-mode-map "p" #'backward-button)
(define-key special-mode-map "f" #'forward-button)
(define-key special-mode-map "b" #'backward-button)
(define-key special-mode-map "n" #'widget-forward)
(define-key special-mode-map "p" #'widget-backward)
(define-key special-mode-map "f" #'widget-forward)
(define-key special-mode-map "b" #'widget-backward)
(provide 'init-keys)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; init-keys.el ends here
