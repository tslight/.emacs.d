;;; term.el --- term configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Term Configuration

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:

(with-eval-after-load 'term
  ;; get unicode characters in ansi-term - https://stackoverflow.com/a/7442266
  (defadvice ansi-term (after advise-ansi-term-coding-system)
    "Get unicode characters in `ansi-term'."
    (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
  (ad-activate 'ansi-term)

  (defadvice term-handle-exit (after term-kill-buffer-on-exit activate)
    "Kill term when shell exits."
    (kill-buffer))

  (setq term-buffer-maximum-size 200000)
  (message "Lazy loaded term :-)"))

(defun my/switch-to-ansi-term ()
  "Open an `ansi-term' if it doesn't already exist.
Otherwise switch to current one."
  (interactive)
  (if (get-buffer "*ansi-term*")
      (switch-to-buffer "*ansi-term*")
    (ansi-term (getenv "SHELL"))))

(global-set-key (kbd "C-c t t") 'my/switch-to-ansi-term)
(global-set-key (kbd "C-c t c") (lambda () (interactive) (ansi-term (getenv "SHELL"))))

(add-hook 'term-exec (lambda () (set-process-coding-system 'utf-8-unix 'utf-8-unix)))
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; term.el ends here
