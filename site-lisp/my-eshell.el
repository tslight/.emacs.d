;;; my-eshell.el --- eshell configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight tslight@pm.me

;;; Code:

(require 'eshell)
(require 'em-smart)
(require 'esh-module)

(add-to-list 'eshell-modules-list 'eshell-tramp)

(defun my/eshell-complete-history ()
  "Insert element from `eshell' history using completion."
  (interactive)
  (let ((hist (ring-elements eshell-history-ring)))
    (insert
     (completing-read "eshell history: " hist nil t))))

;; https://cestlaz.github.io/post/using-emacs-66-eshell-elisp/
(defun my/select-or-create-eshell (arg)
  "Commentary ARG."
  (if (string= arg "New eshell")
      (eshell t)
    (switch-to-buffer arg)))

(require 'cl-lib)
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

(defun my/eshell-recent-dir (&optional arg)
  "Switch to a recent `eshell' directory using completion.

With \\[universal-argument] also open the directory in a `dired'
buffer."
  (interactive "P")
  (let* ((dirs (ring-elements eshell-last-dir-ring))
         (dir (completing-read "Switch to recent dir: " dirs nil t)))
    (insert (concat "cd " dir))
    (eshell-send-input)
    (when arg
      (dired dir))))

(setq eshell-history-size 4096)
(setq eshell-hist-ignoredups t)
(setq eshell-save-history-on-exit t)

(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands t)
(setq eshell-smart-space-goes-to-end t)

(setq eshell-prefer-lisp-functions t)
(setq eshell-prefer-lisp-variables t)

(setq eshell-cd-on-directory t)

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

(define-key eshell-mode-map (kbd "C-c r") 'my/eshell-recent-dir)
(define-key eshell-hist-mode-map (kbd "M-r") 'my/eshell-complete-history)

(provide 'my-eshell)
;;; my-eshell.el ends here
