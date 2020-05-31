;;; my-mark-mitigations.el --- Transient Mark Migigations -*- lexical-binding: t; -*-

;;; Commentary:

;; https://www.masteringemacs.org/article/fixing-mark-commands-transient-mark-mode

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(defun my/exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(defun my/jump-to-mark ()
  "Jump to the local mark, respecting the `mark-ring' order.

This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(defun my/push-mark-no-activate ()
  "Push `point' to `mark-ring', but do not activate the region.

Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(provide 'my-mark-mitigations)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; my-mark-mitigations.el ends here
