;;; isearch.el --- isearch functions -*- lexical-binding: t; -*-

;;; Commentary:

;; isearch functions

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(with-eval-after-load 'isearch
   ;;;###autoload
  (defun my/isearch-exit ()
    "Move point to the start of the matched string."
    (interactive)
    (when (eq isearch-forward t)
      (goto-char isearch-other-end))
    (isearch-exit))
  (define-key isearch-mode-map (kbd "RET") 'my/isearch-exit)

  ;;;###autoload
  (defun my/copy-to-isearch ()
    "Copy up to the search match when searching forward.

When searching backward, copy to the start of the search match."
    (interactive)
    (my/isearch-exit)
    (call-interactively 'kill-ring-save)
    (exchange-point-and-mark))
  (define-key isearch-mode-map (kbd "M-w") 'my/copy-to-isearch)

  ;;;###autoload
  (defun my/kill-to-isearch ()
    "Kill up to the search match when searching forward.

When searching backward, kill to the beginning of the match."
    (interactive)
    (my/isearch-exit)
    (call-interactively 'kill-region))
  (define-key isearch-mode-map (kbd "C-M-w") 'my/kill-to-isearch)

  (define-key isearch-mode-map (kbd "M-/") 'isearch-complete)
  (define-key minibuffer-local-isearch-map (kbd "M-/") 'isearch-complete-edit)

  (when (not (version< emacs-version "27.1"))
    (setq isearch-allow-scroll 'unlimited)
    (setq isearch-yank-on-move 't)
    (setq isearch-lazy-count t)
    (setq lazy-count-prefix-format nil)
    (setq lazy-count-suffix-format " (%s/%s)"))
  (setq search-highlight t)
  (setq search-whitespace-regexp ".*?")
  (setq isearch-lax-whitespace t)
  (setq isearch-regexp-lax-whitespace nil)
  (setq isearch-lazy-highlight t)
  (message "Lazy loaded isearch :-)"))

(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "M-s b") 'multi-isearch-buffers-regexp)
(global-set-key (kbd "M-s f") 'multi-isearch-files-regexp)
(global-set-key (kbd "M-s M-o") 'multi-occur)

(add-hook 'occur-mode-hook 'hl-line-mode)
(define-key occur-mode-map "t" 'toggle-truncate-lines)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; isearch.el ends here
