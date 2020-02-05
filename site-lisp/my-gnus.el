;;; my-gnus.el ---

;;; Commentary:

;; Copyright (C) 2019 
;; Author:  Toby Slight

;;; Code:
;; -*- lexical-binding: t; -*-

(require 'nnir)
(setq gnus-init-file "~/.emacs.d/init.el")
(setq gnus-home-directory "~/.emacs.d/")
(setq gnus-completing-read-function 'gnus-ido-completing-read)
(setq message-directory "~/.emacs.d/mail")
(setq gnus-directory "~/.emacs.d/news")
(setq nnfolder-directory "~/.emacs.d/mail/archive")
(setq gnus-use-full-window nil)
(setq gnus-select-method '(nntp "news.gnus.org"))
(setq gnus-summary-thread-gathering-function
      'gnus-gather-threads-by-subject)
(setq gnus-thread-hide-subtree t)
(setq gnus-thread-ignore-subject t)

(provide 'my-gnus)
;;; my-gnus.el ends here
