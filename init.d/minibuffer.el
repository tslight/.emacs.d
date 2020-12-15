;;; minibuffer.el --- minibuffer/completion configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Extend minibuffer and better default complete engine/settings

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(require 'recentf)
(recentf-mode 1)
(setq recentf-exclude '(;;".*init\.el"
                        ;;".*\/my-.*\.el"
                        "^/var/folders\\.*"
                        "COMMIT_EDITMSG\\'"
                        ".*-autoloads\\.el\\'"
                        "[/\\]\\.elpa/"))
(setq recentf-max-menu-items 128)
(setq recentf-max-saved-items 256)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(setq savehist-save-minibuffer-history 1)

(setq imenu-auto-rescan t)

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

(defvar my/fido-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-k") 'icomplete-fido-kill)
    (define-key map (kbd "C-d") 'icomplete-fido-delete-char)
    (define-key map (kbd "RET") 'icomplete-fido-ret)
    (define-key map (kbd "C-m") 'icomplete-fido-ret)
    (define-key map (kbd "DEL") 'icomplete-fido-backward-updir)
    (define-key map (kbd "C-j") 'icomplete-fido-exit)
    (define-key map (kbd "M-j") 'exit-minibuffer)
    (define-key map (kbd "C-s") 'icomplete-forward-completions)
    (define-key map (kbd "C-r") 'icomplete-backward-completions)
    (define-key map (kbd "<right>") 'icomplete-forward-completions)
    (define-key map (kbd "<left>") 'icomplete-backward-completions)
    map)
  "Keymap used by `my/fido-mode' in the minibuffer.")

(defun my/fido-mode-setup ()
  "Setup `my/fido-mode''s minibuffer."
  (when (and icomplete-mode (icomplete-simple-completing-p))
    (use-local-map (make-composed-keymap my/fido-mode-map
                                         (current-local-map)))
    (setq-local completion-category-defaults nil
                completion-cycle-threshold 3
                completion-flex-nospace nil
                completion-ignore-case t
                completion-pcm-complete-word-inserts-delimiters t
                completion-pcm-word-delimiters "-_./:| "
                completion-show-help nil
                completion-styles '(initials partial-completion flex substring basic)
                completions-detailed t
                completions-format 'one-column
                enable-recursive-minibuffers t
                file-name-shadow-mode 1
                icomplete-compute-delay 0.2
                icomplete-delay-completions-threshold 100
                icomplete-hide-common-prefix nil
                icomplete-max-delay-chars 2
                icomplete-prospects-height 1
                icomplete-separator (propertize " · " 'face 'shadow)
                icomplete-show-matches-on-no-input t
                icomplete-tidy-shadowed-file-names t
                icomplete-with-completion-tables t
                minibuffer-depth-indicate-mode 1
                minibuffer-eldef-shorten-default t
                minibuffer-electric-default-mode 1
                read-answer-short t
                read-buffer-completion-ignore-case t
                read-file-name-completion-ignore-case t
                resize-mini-windows t)))

;;;###autoload
(define-minor-mode my/fido-mode
  "An enhanced `icomplete-mode' that emulates `ido-mode'.

This global minor mode makes minibuffer completion behave
more like `ido-mode' than regular `icomplete-mode'."
  :global t :group 'icomplete
  (remove-hook 'minibuffer-setup-hook #'icomplete-minibuffer-setup)
  (remove-hook 'minibuffer-setup-hook #'my/fido-mode-setup)
  (when my/fido-mode
    (icomplete-mode -1)
    (setq icomplete-mode t)
    (add-hook 'minibuffer-setup-hook #'icomplete-minibuffer-setup)
    (add-hook 'minibuffer-setup-hook #'my/fido-mode-setup)))

(my/fido-mode 1)

(add-hook 'minibuffer-exit-hook (lambda () (setq gc-cons-threshold 800000)))
(add-hook 'minibuffer-setup-hook (lambda () (setq gc-cons-threshold most-positive-fixnum)))
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; minibuffer.el ends here
