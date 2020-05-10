;;; my-eshell.el --- eshell configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight tslight@pm.me

;;; Code:

(require 'eshell)
(require 'em-smart)
(require 'cl-lib)

(defun my/eshell-complete-history ()
  "Insert element from `eshell' history using completion."
  (interactive)
  (let ((hist (ring-elements eshell-history-ring)))
    (insert
     (completing-read "Input history: " hist nil t))))

;; https://cestlaz.github.io/post/using-emacs-66-eshell-elisp/
(defun my/select-or-create-eshell (arg)
  "Commentary ARG."
  (if (string= arg "New eshell")
      (eshell t)
    (switch-to-buffer arg)))

(defun my/eshell-switcher (&optional arg)
  "Commentary ARG."
  (interactive)
  (let* ((buffers (cl-remove-if-not
                   (lambda (n) (eq (buffer-local-value 'major-mode n) 'eshell-mode))
                   (buffer-list)))
         (names (mapcar (lambda (n) (buffer-name n)) buffers))
         (num-buffers (length buffers) )
         (in-eshellp (eq major-mode 'eshell-mode)))
    (cond ((eq num-buffers 0) (eshell (or arg t)))
          ((not in-eshellp) (switch-to-buffer (car buffers)))
          (t (my/select-or-create-eshell
              (completing-read "Select Shell: " (cons "New eshell" names)))))))

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

(my/bind-always "C-c e s" my/eshell-switcher)

(define-key eshell-hist-mode-map (kbd "M-r") 'my/eshell-complete-history)

(provide 'my-eshell)
;;; my-eshell.el ends here
