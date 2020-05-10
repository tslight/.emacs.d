;;; my-isearch.el --- isearch functions -*- lexical-binding: t; -*-

;;; Commentary:

;; isearch functions

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(defun my/isearch-exit ()
  "Move point to the start of the matched string, regardless of
search direction. A.K.A. Vim style."
  (interactive)
  (when (eq isearch-forward t)
    (goto-char isearch-other-end))
  (isearch-exit))
(define-key isearch-mode-map (kbd "<return>") 'my/isearch-exit)

(defun my/copy-to-isearch ()
  "Copy up to the search match when searching forward. When
searching backward, copy to the start of the search match."
  (interactive)
  (my/isearch-exit)
  (call-interactively 'kill-ring-save)
  (exchange-point-and-mark))
(define-key isearch-mode-map (kbd "C-w") 'my/copy-to-isearch)

(defun my/kill-to-isearch ()
  "Kill up to the search match when searching forward. When
searching backward, kill to the beginning of the match."
  (interactive)
  (my/isearch-exit)
  (call-interactively 'kill-region))
(define-key isearch-mode-map (kbd "M-w") 'my/kill-to-isearch)

(provide 'my-isearch)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; my-isearch.el ends here
