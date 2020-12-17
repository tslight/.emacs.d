;;; ido.el --- ido configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Ido Mode Configuration

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(setq ido-use-virtual-buffers 't) ;; show recent files too
(setq ido-create-new-buffer 'always)
(setq ido-enable-prefix t)
(setq ido-enable-flex-matching 't)
(setq ido-auto-merge-work-directories-length -1)
;; (setq ido-use-filename-at-point 'ffap-guesser)
(setq ido-use-filename-at-point 'nil)
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

(defun my/ido-find-recentf ()
  "Use `ido-completing-read' to `find-file' a recent file."
  (interactive)
  (recentf-mode 1)
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

(defun my/ido-mx ()
  "Use function `ido-mode-completing-read' for `execute-extended-command'."
  (interactive)
  (call-interactively
                  (intern
                   (ido-completing-read
                    "M-x "
                    (all-completions "" obarray 'commandp)))))

(global-set-key (kbd "C-c r") 'my/ido-find-recentf)
(global-set-key (kbd "C-M-y") 'my/ido-kill-ring)
(global-set-key (kbd "M-x") 'my/ido-mx)

(define-key icomplete-minibuffer-map (kbd "C-f") 'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "C-b") 'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "C-n") 'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "C-p") 'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "C-s") 'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "C-r") 'icomplete-backward-completions)

(add-hook 'ido-minibuffer-setup-hook (lambda () (set (make-local-variable 'truncate-lines) nil)))
(add-hook 'ido-setup-hook (lambda () (define-key ido-completion-map [tab] 'ido-complete)))
(add-hook 'ido-setup-hook 'my/ido-keys)

(ido-mode 1)
(ido-everywhere 1)

;; stop ido suggestion when doing a save-as
(define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; ido.el ends here
