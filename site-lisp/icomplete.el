;;; icomplete.el --- icomplete configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; icomplete stuff

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
;;;###autoload
(defun my/icomplete-styles ()
  "Set icomplete styles based on Emacs version."
  (if (version< emacs-version "27")
      (setq-local completion-styles '(initials partial-completion substring basic))
    (setq-local completion-styles '(initials partial-completion flex substring basic))))
(add-hook 'icomplete-minibuffer-setup-hook 'my/icomplete-styles)

(if (version< emacs-version "27")
    (icomplete-mode)
  (fido-mode))

(setq icomplete-delay-completions-threshold 100)
(setq icomplete-max-delay-chars 2)
(setq icomplete-compute-delay 0.2)
(setq icomplete-show-matches-on-no-input t)
(setq icomplete-hide-common-prefix nil)
(setq icomplete-prospects-height 1)
;; (setq icomplete-separator "\n")
(setq icomplete-separator (propertize " · " 'face 'shadow))
(setq icomplete-with-completion-tables t)
(setq icomplete-tidy-shadowed-file-names t)
(setq icomplete-in-buffer t)

(if (version< emacs-version "27")
    (define-key icomplete-minibuffer-map (kbd "C-j") 'icomplete-fido-exit))

(define-key icomplete-minibuffer-map (kbd "M-j") 'exit-minibuffer)
(define-key icomplete-minibuffer-map (kbd "C-n") 'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "C-p") 'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "<up>") 'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "<down>") 'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "<left>") 'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "<right>") 'icomplete-forward-completions)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; icomplete.el ends here
