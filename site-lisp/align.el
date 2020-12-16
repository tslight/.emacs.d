;;; align.el --- aligning functions -*- lexical-binding: t; -*-

;;; Commentary:

;; aligning functions

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
;;;###autoload
(defun my/align-symbol (begin end symbol)
  "Align any SYMBOL in region (between BEGIN and END)."
  (interactive "r\nsEnter align symbol: ")
  (align-regexp begin end (concat "\\(\\s-*\\)" symbol) 1 1))
(global-set-key (kbd "C-c a") 'my/align-symbol)

;;;###autoload
(defun my/align-equals (begin end)
  "Align equals in region (between BEGIN and END)."
  (interactive "r")
  (my/align-symbol begin end "="))
(global-set-key (kbd "C-c =") 'my/align-equals)

;;;###autoload
(defun my/align-colon (begin end)
  "Align colons in region (between BEGIN and END)."
  (interactive "r")
  (my/align-symbol begin end ":"))
(global-set-key (kbd "C-c :") 'my/align-colon)

;;;###autoload
(defun my/align-numbers (begin end)
  "Align numbers in region (between BEGIN and END)."
  (interactive "r")
  (my/align-symbol begin end "[0-9]+"))
(global-set-key (kbd "C-c #") 'my/align-numbers)

(defadvice align-regexp (around align-regexp-with-spaces activate)
  "Force alignment commands to use spaces, not tabs."
  (let ((indent-tabs-mode nil))
    ad-do-it))
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; align.el ends here
