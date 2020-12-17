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

;;;###autoload
(defun my/switch-to-ansi-term ()
  "Open an `ansi-term' if it doesn't already exist.
Otherwise switch to current one."
  (interactive)
  (if (get-buffer "*ansi-term*")
      (switch-to-buffer "*ansi-term*")
    (ansi-term (getenv "SHELL"))))

;;;###autoload
(defun my/switch-to-ansi-term-other-window()
  "Does what it states on the tin!"
  (interactive)
  (split-window-sensibly)
  (other-window 1)
  (my/switch-to-ansi-term))

;;;###autoload
(defun my/ansi-term ()
  "Opens shell from $SHELL environmental variable in `ansi-term'."
  (interactive)
  (ansi-term (getenv "SHELL")))

;;;###autoload
(defun my/ansi-term-other-window ()
  "Opens default $SHELL `ansi-term' in another window."
  (interactive)
  (split-window-sensibly)
  (other-window 1)
  (my/ansi-term))

(autoload 'term "term" nil t)
(autoload 'ansi-term "term" nil t)
(global-set-key (kbd "C-c t") 'my/switch-to-ansi-term)
(global-set-key (kbd "C-c 4 t") 'my/switch-to-ansi-term-other-window)
(global-set-key (kbd "C-c C-t") 'my/ansi-term)
(global-set-key (kbd "C-c 4 C-t") 'my/ansi-term-other-window)

(add-hook 'term-exec (lambda () (set-process-coding-system 'utf-8-unix 'utf-8-unix)))
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; term.el ends here
