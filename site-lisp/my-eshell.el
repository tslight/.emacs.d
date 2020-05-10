;;; my-eshell.el --- eshell configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight tslight@pm.me
;; Version: 0.0.1
;; Package-Requires: nil
;; URL: https://github.com/tslight/.emacs.d

;;; Code:

(require 'eshell)
(require 'em-smart)

(setq eshell-history-size 2048)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

;; https://www.emacswiki.org/emacs/EshellPrompt
(setq eshell-prompt-regexp "^[^#$\n]*[#$] "
      eshell-prompt-function
      (lambda nil
        (concat
         (user-login-name) "@" (system-name) " "
         (if (string= (eshell/pwd) (getenv "HOME"))
             "~" (eshell/basename (eshell/pwd)))
         "\n"
         (if (= (user-uid) 0) "# " "$ "))))

(my/bind-always "C-c e s" eshell)

(provide 'my-eshell)
;;; my-eshell.el ends here
