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

;; https://www.emacswiki.org/emacs/EshellAlias
(defun eshell-load-bash-aliases ()
  "Convert bash aliases to eshell aliases."
  (interactive)
  (progn
    (message "Parsing aliases")
    (shell-command "alias" "bash-aliases" "bash-errors")
    (switch-to-buffer "bash-aliases")
    (replace-match "alias " "")
    (goto-char 1)
    (replace-match "='" " ")
    (goto-char 1)
    (replace-match "'\n" "\n")
    (goto-char 1)
    (let ((alias-name) (command-string) (alias-list))
      (while (not (eobp))
        (while (not (char-equal (char-after) 32))
          (forward-char 1))
        (setq alias-name
              (buffer-substring-no-properties (line-beginning-position) (point)))
        (forward-char 1)
        (setq command-string
              (buffer-substring-no-properties (point) (line-end-position)))
        (setq alias-list (cons (list alias-name command-string) alias-list))
        (forward-line 1))
      (setq eshell-command-aliases-list alias-list))
    (if (get-buffer "bash-aliases")(kill-buffer "bash-aliases"))
    (if (get-buffer "bash-errors")(kill-buffer "bash-errors"))))

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
         (if (= (user-uid) 0) "# " "$ "))))

(my/bind-always "C-c e s" eshell)

(provide 'my-eshell)
;;; my-eshell.el ends here
