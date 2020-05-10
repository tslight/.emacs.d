;;; my-eshell.el --- eshell configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; http://www.howardism.org/Technical/Emacs/eshell-fun.html
;; https://masteringemacs.org/article/complete-guide-mastering-eshell
;; https://cestlaz.github.io/post/using-emacs-66-eshell-elisp/

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight tslight@pm.me

;;; Code:
(require 'cl-lib)
(require 'eshell)
(require 'em-pred)
(require 'em-hist)
(require 'em-smart)
(require 'em-tramp)
(require 'esh-module)

(defun my/eshell-here ()
  "Opens up a new eshell in the current directory.

The eshell is renamed to match that directory to make multiple
eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (name (car (last (split-string parent "/" t)))))
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))))

(defun my/select-or-create-eshell (arg)
  "Commentary ARG."
  (if (string= arg "New eshell")
      (my/eshell-here)
    (switch-to-buffer arg)))

(defun my/eshell-switcher ()
  "Open an eshell or switch to an existing one."
  (interactive)
  (let* ((buffers (cl-remove-if-not
                   (lambda (n) (eq (buffer-local-value 'major-mode n) 'eshell-mode))
                   (buffer-list)))
         (names (mapcar (lambda (n) (buffer-name n)) buffers))
         (num-buffers (length buffers) )
         (in-eshellp (eq major-mode 'eshell-mode)))
    (cond ((eq num-buffers 0) (my/eshell-here))
          ((not in-eshellp) (switch-to-buffer (car buffers)))
          (t (my/select-or-create-eshell
              (completing-read "Select Shell: " (cons "New eshell" names)))))))

(defun my/eshell-complete-history ()
  "Insert element from `eshell' history using completion."
  (interactive)
  (let ((hist (ring-elements eshell-history-ring)))
    (insert
     (completing-read "eshell history: " hist nil t))))

(defun my/eshell-recent-dir (&optional arg)
  "Switch to a recent `eshell' directory using completion.

With ARG also open the directory in a `dired' buffer."
  (interactive "P")
  (let* ((dirs (ring-elements eshell-last-dir-ring))
         (dir (completing-read "Switch to recent dir: " dirs nil t)))
    (insert (concat "cd " dir))
    (eshell-send-input)
    (when arg
      (dired dir))))

(defun my/eshell-directory-children ()
  "Recursive `eshell/cd' to subdirectory.

This command has the potential for infinite recursion: use it
wisely or prepare to use `eshell-interrupt-process'."
  (interactive)
  (let* ((dir (eshell/pwd))
         (contents (directory-files-recursively dir ".*" t nil nil))
         ;; (contents (directory-files dir t))
         (find-directories (mapcar (lambda (x)
                                     (when (file-directory-p x)
                                       (abbreviate-file-name x)))
                                   contents))
         (subdirs (delete nil find-directories))
         (cands (cl-remove-if (lambda (x) (string-match-p "\\.git" x)) subdirs))
         (selection (completing-read "Go to directory: " cands nil t)))
    (insert (concat "cd " selection))
    (eshell-send-input)))

;; smart stuff
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

(setq eshell-history-size 4096)
(setq eshell-hist-ignoredups t)
(setq eshell-save-history-on-exit t)

(setq eshell-prefer-lisp-functions t)
(setq eshell-prefer-lisp-variables t)

(setq eshell-cd-on-directory t)

;; https://www.emacswiki.org/emacs/EshellPrompt
(setq eshell-prompt-regexp "^[^#$\n]*[#$] "
      eshell-prompt-function
      (lambda nil
        (concat
         (propertize (user-login-name) 'face `(:foreground "green" ))
         (propertize "@" 'face `(:foreground "yellow"))
         (propertize (system-name) `face `(:foreground "green"))
         (propertize ":" 'face `(:foreground "yellow"))
         (if (string= (eshell/pwd) (getenv "HOME"))
             (propertize "~" 'face `(:foreground "magenta"))
           (propertize (eshell/basename (eshell/pwd)) 'face `(:foreground "magenta")))
         (propertize (ignore-errors (format " (%s)"
                                            (vc-responsible-backend default-directory)))
                     'face `(:foreground "cyan"))
         "\n"
         (if (= (user-uid) 0)
             (propertize "#" 'face `(:foreground "red"))
           (propertize "$" 'face `(:foreground "yellow")))
         (propertize " " 'face `(:foreground "white"))))
      eshell-highlight-prompt nil)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;; (add-hook 'eshell-preoutput-filter-functions 'ansi-color-filter-apply)
(add-hook 'eshell-preoutput-filter-functions 'ansi-color-apply)

;; https://www.8t8.us/blog/2016/06/05/enabling-eshell-smart-display-mode.html
(add-hook 'eshell-mode-hook (lambda () (eshell-smart-initialize)))

(add-to-list 'eshell-modules-list 'eshell-tramp) ;; no sudo password with ~/.authinfo

(my/bind-always "C-c C-e" my/eshell-switcher)

(define-key eshell-mode-map (kbd "C-c r") 'my/eshell-recent-dir)
(define-key eshell-mode-map (kbd "C-c D") 'my/eshell-directory-children)
(define-key eshell-hist-mode-map (kbd "M-r") 'my/eshell-complete-history)

(provide 'my-eshell)
;;; my-eshell.el ends here
