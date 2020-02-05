;;; my-advice.el ---

;;; Commentary:

;; Copyright (C) 2019 
;; Author:  Toby Slight

;;; Code:
;; -*- lexical-binding: t; -*-

(defadvice align-regexp (around align-regexp-with-spaces activate)
  "Force alignment commands to use spaces, not tabs."
  (let ((indent-tabs-mode nil))
    ad-do-it))

(defadvice load-theme (before theme-dont-propagate activate)
  "Disable theme before loading new one."
  (mapc #'disable-theme custom-enabled-themes))

(defadvice term-handle-exit (after term-kill-buffer-on-exit activate)
    "Kill term when shell exits."
    (kill-buffer))

(defadvice text-scale-increase (around all-buffers (arg) activate)
  "Make all buffers increase font size with `text-scale-increase'."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      ad-do-it)))

(provide 'my-advice)
;;; my-advice.el ends here
