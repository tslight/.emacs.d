;;; ediff.el --- ediff configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Ediff Configuration

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(with-eval-after-load 'ediff
  (setq ediff-diff-options "-w")
  (setq ediff-keep-variants nil)
  (setq ediff-make-buffers-readonly-at-startup nil)
  (setq ediff-merge-revisions-with-ancestor t)
  (setq ediff-show-clashes-only t)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo)

  ;; https://emacs.stackexchange.com/a/24602
  ;;;###autoload
  (defun disable-y-or-n-p (orig-fun &rest args)
    "Advise ORIG-FUN with ARGS so it dynamically rebinds `y-or-n-p'."
    (cl-letf (((symbol-function 'y-or-n-p) (lambda () t)))
      (apply orig-fun args)))

  (advice-add 'ediff-quit :around #'disable-y-or-n-p)

  (message "Lazy loaded ediff :-)"))
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; ediff.el ends here
