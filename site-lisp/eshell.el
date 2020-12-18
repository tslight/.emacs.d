;;; eshell.el --- eshell configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Eshell Configuration initialisation

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(with-eval-after-load 'eshell

  ;;;###autoload
  (defun my/eshell-complete-recent-dir (&optional arg)
    "Switch to a recent `eshell' directory using completion.
With \\[universal-argument] also open the directory in a `dired'
buffer."
    (interactive "P")
    (let* ((dirs (ring-elements eshell-last-dir-ring))
           (dir (completing-read "Switch to recent dir: " dirs nil t)))
      (insert dir)
      (eshell-send-input)
      (when arg
        (dired dir))))

  ;;;###autoload
  (defun my/eshell-complete-history ()
    "Insert element from `eshell' history using completion."
    (interactive)
    (let ((hist (ring-elements eshell-history-ring)))
      (insert
       (completing-read "Input history: " hist nil t))))

  ;;;###autoload
  (defun my/eshell-prompt ()
    "Custom eshell prompt."
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

  ;; https://www.emacswiki.org/emacs/EshellPrompt
  (setq
   eshell-cd-on-directory t
   eshell-destroy-buffer-when-process-dies t
   eshell-highlight-prompt nil
   eshell-hist-ignoredups t
   eshell-history-size 4096
   eshell-ls-use-colors t
   eshell-prefer-lisp-functions t
   eshell-prefer-lisp-variables t
   eshell-prompt-regexp "^[^#$\n]*[#$] "
   eshell-prompt-function 'my/eshell-prompt
   eshell-review-quick-commands nil
   eshell-save-history-on-exit t
   eshell-smart-space-goes-to-end t
   eshell-where-to-jump 'begin)

  (add-to-list 'eshell-modules-list 'eshell-tramp) ;; no sudo password with ~/.authinfo
  (add-hook 'eshell-preoutput-filter-functions 'ansi-color-apply)

  (defun my/eshell-keys()
    (define-key eshell-mode-map (kbd "M-r") 'my/eshell-complete-history)
    (define-key eshell-mode-map (kbd "C-=") 'my/eshell-complete-recent-dir))

  (add-hook 'eshell-mode-hook 'my/eshell-keys)

  (message "Lazy loaded eshell :-)"))

;;;###autoload
(defun my/eshell-other-window ()
  "Open an `eshell' in another window."
  (interactive)
  (split-window-sensibly)
  (other-window 1)
  (eshell))

(autoload 'eshell "eshell" nil t)
(global-set-key (kbd "C-c e") 'eshell)
(global-set-key (kbd "C-c 4 e") 'my/eshell-other-window)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; eshell.el ends here
