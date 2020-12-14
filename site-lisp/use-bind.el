;;; my-bind.el --- my bind key config -*- lexical-binding: t; -*-

;;; Commentary:

;; my bind key configuration

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(use-package bind-key
  :ensure t
  :bind*
  ("C-c ;" . comment-line)
  ("C-z" . zap-up-to-char)
  ("M-z" . zap-up-char)
  ("C-x M-t" . transpose-regions)
  ("C-x M-p" . transpose-paragraphs)
  ("C-c e b" . eval-buffer)
  ("C-c e e" . pp-eval-last-sexp)
  ("C-c e m" . lisp-interaction-mode)
  ("C-c M-t a" . toggle-text-mode-autofill)
  ("C-c M-t d E" . toggle-debug-on-entry)
  ("C-c M-t d e" . toggle-debug-on-error)
  ("C-c M-t d q" . toggle-debug-on-quit)
  ("C-c M-t t" . toggle-truncate-lines)
  ("C-c M-t m" . menu-bar-mode)
  ("C-c h n" . highlight-changes-next-change)
  ("C-c h p" . highlight-changes-previous-change)
  ("C-x c" . save-buffers-kill-emacs)
  ("C-x C-b" . ibuffer)
  ("C-x M-k" . kill-buffer)
  ("C-x t t" . tab-bar-select-tab-by-name)
  ("C-x t c" . tab-bar-new-tab)
  ("C-x t k" . tab-bar-close-tab)
  ("C-x t n" . tab-bar-switch-to-next-tab)
  ("C-x t p" . tab-bar-switch-to-prev-tab)
  ("C-x t l" . tab-bar-switch-to-recent-tab)
  ("C-<f10>" . toggle-frame-maximized)
  ("C-<f11>" . toggle-frame-fullscreen)
  ("C-s-f" . toggle-frame-fullscreen)
  ("C-s-m" . toggle-frame-maximized)
  ("C-c C-r" . recentf-open-files)
  ("C-c M-d r" . desktop-read)
  ("C-c M-d s" . desktop-save)
  ("S-<f10>" . menu-bar-mode)
  ("C-c v" . scroll-other-window-down)
  ("C-c C-/" . winner-undo)
  ("C-c C-?" . winner-redo)
  ("C-c M-c" . calculator)
  ("C-c c" . calc)
  ("C-x j" . jump-to-register)
  ("C-x p" . pop-to-mark-command)
  ("M-SPC" . cycle-spacing)
  ("M-/" . hippie-expand)
  :bind
  ("C-r" . isearch-backward-regexp)
  ("C-s" . isearch-forward-regexp)
  ("C-M-r" . multi-isearch-buffers-regexp)
  ("C-M-s" . multi-isearch-files-regexp)
  (:map special-mode-map
        ("n" . forward-button)
        ("p" . backward-button)
        ("f" . forward-button)
        ("b" . backward-button)
        ("n" . widget-forward)
        ("p" . widget-backward)
        ("f" . widget-forward)
        ("b" . widget-backward)))

(provide 'my-bind)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; use-bind.el ends here
