;;; gnus.el --- gnus configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Gnus Configuration

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(with-eval-after-load 'gnus
  (require 'nnir)
  (setq gnus-init-file "~/.emacs.d/init.el")
  (setq gnus-home-directory "~/.emacs.d/")
  (setq message-directory "~/.emacs.d/mail")
  (setq gnus-directory "~/.emacs.d/news")
  (setq nnfolder-directory "~/.emacs.d/mail/archive")
  (setq gnus-use-full-window nil)
  (setq gnus-select-method '(nntp "news.gnus.org"))
  (setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)
  (setq gnus-thread-hide-subtree t)
  (setq gnus-thread-ignore-subject t)

  (message "Lazy loaded gnus :-)"))
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; gnus.el ends here
