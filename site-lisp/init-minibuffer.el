;;; init-minibuffer.el --- minibuffer/completion configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Extend minibuffer and better default complete engine/settings

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(defun my/icomplete-recentf ()
  "Show a list of recent files."
  (interactive)
  (let* ((all-files recentf-list)
         (list1 (mapcar (lambda (x) (file-name-nondirectory x) x) all-files))
         (list2 (mapcar #'substring-no-properties list1))
         (list3 (mapcar #'abbreviate-file-name list2))
         (list4 (cl-remove-duplicates list3 :test #'string-equal)))
    (find-file (completing-read "Recent Files: " list4 nil t))))
(global-set-key (kbd "C-c r") 'my/icomplete-recentf)

(defun my/icomplete-styles ()
  "Set icomplete styles based on Emacs version."
  (if (version< emacs-version "27")
      (setq-local completion-styles '(initials partial-completion substring basic))
    (setq-local completion-styles '(initials partial-completion flex substring basic))))
(add-hook 'icomplete-minibuffer-setup-hook 'my/icomplete-styles)

(recentf-mode 1)
(setq recentf-exclude '(;;".*init\.el"
                        ;;".*\/my-.*\.el"
                        "^/var/folders\\.*"
                        "COMMIT_EDITMSG\\'"
                        ".*-autoloads\\.el\\'"
                        "[/\\]\\.elpa/"))
(setq recentf-max-menu-items 128)
(setq recentf-max-saved-items 256)

(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(setq savehist-save-minibuffer-history 1)

(setq completion-category-defaults nil)
(setq completion-cycle-threshold 3)
(setq completion-flex-nospace nil)
(setq completion-pcm-complete-word-inserts-delimiters t)
(setq completion-pcm-word-delimiters "-_./:| ")
(setq completion-show-help nil)
(setq completion-ignore-case t)
(setq completions-format 'one-column)
(setq completions-detailed t)

(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq enable-recursive-minibuffers t)
(setq read-answer-short t)
(setq resize-mini-windows t)
(setq minibuffer-eldef-shorten-default t)
(setq file-name-shadow-mode 1)
(setq minibuffer-depth-indicate-mode 1)
(setq minibuffer-electric-default-mode 1)

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

(setq imenu-auto-rescan t)

(add-hook 'minibuffer-exit-hook (lambda () (setq gc-cons-threshold 800000)))
(add-hook 'minibuffer-setup-hook (lambda () (setq gc-cons-threshold most-positive-fixnum)))

(provide 'init-minibuffer)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; init-minibuffer.el ends here
