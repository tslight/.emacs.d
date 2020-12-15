;;; minibuffer.el --- minibuffer/completion configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Extend minibuffer and better default complete engine/settings

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(with-eval-after-load 'recentf
  (setq recentf-exclude '("^/var/folders\\.*"
                          "COMMIT_EDITMSG\\'"
                          ".*-autoloads\\.el\\'"
                          "[/\\]\\.elpa/"))
  (setq recentf-max-menu-items 128)
  (setq recentf-max-saved-items 256)

  (global-set-key (kbd "C-c C-r") 'recentf-open-files)

  (defun my/completing-recentf ()
    "Show a list of recent files."
    (interactive)
    (let* ((all-files recentf-list)
           (list1 (mapcar (lambda (x) (file-name-nondirectory x) x) all-files))
           (list2 (mapcar #'substring-no-properties list1))
           (list3 (mapcar #'abbreviate-file-name list2))
           (list4 (cl-remove-duplicates list3 :test #'string-equal)))
      (find-file (completing-read "Recent Files: " list4 nil t))))

  (global-set-key (kbd "C-c r") 'my/completing-recentf)

  (message "Lazy loaded recentf :-)"))

(recentf-mode 1)

(with-eval-after-load 'uniquify
  (setq uniquify-buffer-name-style 'forward)
  (message "Lazy loaded uniquify :-)"))

(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(setq savehist-save-minibuffer-history 1)

(setq imenu-auto-rescan t)

(setq completion-category-defaults nil)
(setq completion-cycle-threshold 3)
(setq completion-flex-nospace nil)
(setq completion-ignore-case t)
(setq completion-pcm-complete-word-inserts-delimiters t)
(setq completion-pcm-word-delimiters "-_./:| ")
(setq completion-show-help nil)
(setq completions-detailed t)
(setq completions-format 'one-column)

(setq enable-recursive-minibuffers t)
(setq file-name-shadow-mode 1)
(setq minibuffer-depth-indicate-mode 1)
(setq minibuffer-eldef-shorten-default t)
(setq minibuffer-electric-default-mode 1)
(setq read-answer-short t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq resize-mini-windows t)

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
(setq icomplete-separator (propertize " · " 'face 'shadow))
(setq icomplete-with-completion-tables t)
(setq icomplete-tidy-shadowed-file-names t)

(if (version< emacs-version "27")
    (define-key icomplete-minibuffer-map (kbd "C-j") 'icomplete-fido-exit))

(define-key icomplete-minibuffer-map (kbd "M-j") 'exit-minibuffer)
(define-key icomplete-minibuffer-map (kbd "C-n") 'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "C-p") 'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "<up>") 'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "<down>") 'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "<left>") 'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "<right>") 'icomplete-forward-completions)

(add-hook 'minibuffer-exit-hook (lambda () (setq gc-cons-threshold 800000)))
(add-hook 'minibuffer-setup-hook (lambda () (setq gc-cons-threshold most-positive-fixnum)))
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; minibuffer.el ends here
