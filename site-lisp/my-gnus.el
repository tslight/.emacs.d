;;; my-gnus.el --- gnus settings  -*- lexical-binding: t; -*-

;;; Commentary:

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight tslight@pm.me
;; Version: 0.0.1
;; Package-Requires: nil
;; URL: https://github.com/tslight/.emacs.d

;;; Code:
(require 'nnir)
(setq gnus-init-file "~/.emacs.d/init.el")
(setq gnus-home-directory "~/.emacs.d/")
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
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; my-gnus.el ends here
