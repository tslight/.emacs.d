;;; my-ediff.el --- ediff settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight tslight@pm.me
;; Version: 0.0.1
;; Package-Requires: nil
;; URL: https://github.com/tslight/.emacs.d

;;; Code:
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-diff-options "-w")

(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; https://emacs.stackexchange.com/a/24602
(defun disable-y-or-n-p (orig-fun &rest args)
  "Advise ORIG-FUN with ARGS so it dynamically rebinds `y-or-n-p'."
  (cl-letf (((symbol-function 'y-or-n-p) (lambda () t)))
    (apply orig-fun args)))

(advice-add 'ediff-quit :around #'disable-y-or-n-p)

(provide 'my-ediff)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; my-ediff.el ends here
