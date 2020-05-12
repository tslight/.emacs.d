;;; my-settings.el --- my-settings  -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs Configuration

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(setq abbrev-file-name (concat user-emacs-directory "abbrevs"))
(setq save-abbrevs 'silently)

(setq auto-window-vscroll nil)

(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
(setq auto-save-timeout 5)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t) ;; copy files, don't rename them.
(setq delete-old-versions t)
(setq kept-new-versions 12)
(setq kept-old-versions 12)

(setq ring-bell-function 'ignore)
(setq visible-bell 1)

(setq select-enable-clipboard t)
(setq select-enable-primary t)
(setq x-select-enable-clipboard-manager nil)
(setq save-interprogram-paste-before-kill t)

(setq display-line-numbers 'relative)

(setq doc-view-continuous t)
(setq doc-view-resolution 300)

(global-subword-mode 1) ;; move by camel case, etc
(global-auto-revert-mode 1) ;; reload if file changed on disk
(pending-delete-mode 1) ;; remove selected region if typing
(set-default 'truncate-lines t)
(setq-default fill-column 79)
(setq backward-delete-char-untabify-method 'all)
(setq create-lockfiles nil) ;; prevent creation of .#myfile.ext
(setq require-final-newline t) ;; useful for crontab
(setq set-mark-command-repeat-pop t) ;; repeating C-SPC after popping, pops it
(show-paren-mode 1)

(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(setq coding-system-for-read 'utf-8-unix)
(setq coding-system-for-write 'utf-8-unix)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

(setq epa-file-cache-passphrase-for-symmetric-encryption t)
(setf epg-pinentry-mode 'loopback)

(global-highlight-changes-mode)
(setq highlight-changes-visibility-initial-state nil)

(setq history-length t)
(setq history-delete-duplicates t)
(setq bookmark-save-flag 1) ;; always save bookmarks to file
(save-place-mode 1)
(setq save-place-file (concat user-emacs-directory "saveplace.el"))

(setq load-prefer-newer t) ;; if init.elc is older, use newer init.el

(setq compilation-scroll-output 'first-error)
(setq custom-file (make-temp-file "emacs-custom"))
(setq disabled-command-function nil) ;; enable all "advanced" features
(setq message-log-max 10000)
(setq apropos-do-all t) ;; doesn't seem to be documented anywhere..

(setq mouse-yank-at-point t)

(setq scroll-step 4)
(setq scroll-margin 6)
(setq scroll-conservatively 8)
(setq scroll-preserve-screen-position t)

(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'fundamental-mode)

(setq tramp-backup-directory-alist backup-directory-alist)
(setq tramp-default-method "ssh")
(setf tramp-persistency-file-name (concat temporary-file-directory "tramp-" (user-login-name)))

;; (setq password-cache t) ; enable password caching
;; (setq password-cache-expiry 3600) ; for one hour (time in secs)

(setq undo-limit 150000)
(setq undo-strong-limit 300000)

(setq user-full-name "Toby Slight")
(setq user-mail-address "tslight@pm.me")

(setq vc-follow-symlinks t)
(setq vc-make-backup-files t)
(setq version-control t)

(when (fboundp 'winner-mode) (winner-mode 1))
(setq split-width-threshold 160)
(setq split-height-threshold 80)

(fset 'yes-or-no-p 'y-or-n-p) ;; never have to type full word
(setq confirm-kill-emacs 'y-or-n-p)

;; supposedly increases minibuffer performance
(add-hook 'minibuffer-exit-hook (lambda () (setq gc-cons-threshold 800000)))
(add-hook 'minibuffer-setup-hook (lambda () (setq gc-cons-threshold most-positive-fixnum)))

(provide 'my-settings)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; my-settings.el ends here
