;;; marks.el --- Transient Mark Migigations -*- lexical-binding: t; -*-

;;; Commentary:

;; https://www.masteringemacs.org/article/fixing-mark-commands-transient-mark-mode

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
;;;###autoload
(defun my/exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

;;;###autoload
(defun my/jump-to-mark ()
  "Jump to the local mark, respecting the `mark-ring' order.

This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

;;;###autoload
(defun my/push-mark-no-activate ()
  "Push `point' to `mark-ring', but do not activate the region.

Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(global-set-key (kbd "C-c SPC p") 'my/push-mark-no-activate)
(global-set-key (kbd "C-c SPC j") 'my/jump-to-mark)
(global-set-key (kbd "C-c SPC x") 'my/exchange-point-and-mark-no-activate)

(global-set-key (kbd "C-x p") 'pop-to-mark-command)

(add-hook 'before-save-hook 'my/push-mark-no-activate)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; marks.el ends here
