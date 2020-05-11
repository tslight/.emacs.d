;;; my-change-numbers.el --- change numbers -*- lexical-binding: t; -*-

;;; Commentary:

;; change numbers

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(defun my/change-number-at-point (change)
  "Change a number by CHANGE amount."
  (let ((number (number-at-point))
        (point (point)))
    (when number
      (progn
        (forward-word)
        (search-backward (number-to-string number))
        (replace-match (number-to-string (funcall change number)))
        (goto-char point)))))

(defun my/increment-number-at-point ()
  "Increment number at point."
  (interactive)
  (my/change-number-at-point '1+))

(defun my/decrement-number-at-point ()
  "Decrement number at point."
  (interactive)
  (my/change-number-at-point '1-))

(provide 'my-change-numbers)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; my-change-numbers.el ends here
