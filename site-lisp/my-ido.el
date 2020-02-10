;;; my-ido.el ---

;;; Commentary:

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight tslight@pm.me

;;; Code:
;; -*- lexical-binding: t; -*-
(require 'ido)

(setq ido-use-virtual-buffers 't) ;; show recent files too
(setq ido-auto-merge-work-directories-length -1)
(setq ido-create-new-buffer 'always)
(setq ido-enable-prefix t)
(setq ido-enable-flex-matching 't)
(setq ido-everywhere t)
(setq ido-use-filename-at-point 'ffap-guesser)

(setq ido-decorations (quote ("\n--> "
			      " "
			      "\n    "
			      "\n    ..."
			      "["  "]"
			      "  [No match]"
			      "  [Matched]"
			      "  [Not readable]"
			      "  [Too big]"
			      "  [Confirm]")))

(require 'icomplete)
(setq icomplete-separator "\n")
(setq icomplete-hide-common-prefix 'nil)
(setq icomplete-in-buffer t)

(recentf-mode 1)
(defun my/ido-find-recentf ()
  "Use `ido-completing-read' to `find-file' a recent file."
  (interactive)
  (if (find-file (ido-completing-read "Recent: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(defun my/ido-kill-ring ()
  "Use `ido-completing-read' to choose a `yank' from the `kill-ring'."
  (interactive)
  (let ((my-yank (ido-completing-read "Kill Ring: " kill-ring)))
    (insert my-yank)))

(defun my/ido-keys ()
  "Custom ido keybindings."
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))

(define-key my/keymap (kbd "C-c r") 'my/ido-find-recentf)
(define-key my/keymap (kbd "C-M-y") 'my/ido-kill-ring)
(define-key my/keymap (kbd "M-x") (lambda () (interactive)
				    (call-interactively
				     (intern
				      (ido-completing-read
				       "M-x "
				       (all-completions "" obarray 'commandp))))))

(define-key icomplete-minibuffer-map (kbd "C-f") 'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "C-b") 'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "C-n") 'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "C-p") 'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "C-s") 'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "C-r") 'icomplete-backward-completions)

(add-hook 'ido-minibuffer-setup-hook (lambda () (set (make-local-variable 'truncate-lines) nil)))
(add-hook 'ido-setup-hook (lambda () (define-key ido-completion-map [tab] 'ido-complete)))
(add-hook 'ido-setup-hook 'my/ido-keys)

(icomplete-mode 1)
(ido-mode 1)

(provide 'my-ido)
;;; my-ido.el ends here
