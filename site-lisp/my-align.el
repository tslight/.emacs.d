;;; my-align.el --- aligning functions -*- lexical-binding: t; -*-

;;; Commentary:

;; aligning functions

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(defun my/align-symbol (begin end symbol)
  "Align any SYMBOL in region (between BEGIN and END)."
  (interactive "r\nsEnter align symbol: ")
  (align-regexp begin end (concat "\\(\\s-*\\)" symbol) 1 1))

(defun my/align-equals (begin end)
  "Align equals in region (between BEGIN and END)."
  (interactive "r")
  (my/align-symbol begin end "="))

(defun my/align-colon (begin end)
  "Align colons in region (between BEGIN and END)."
  (interactive "r")
  (my/align-symbol begin end ":"))

(defun my/align-numbers (begin end)
  "Align numbers in region (between BEGIN and END)."
  (interactive "r")
  (my/align-symbol begin end "[0-9]+"))

(defadvice align-regexp (around align-regexp-with-spaces activate)
  "Force alignment commands to use spaces, not tabs."
  (let ((indent-tabs-mode nil))
    ad-do-it))

(provide 'my-align)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; my-align.el ends here
