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

(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(setq savehist-save-minibuffer-history 1)
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

(setq c-default-style "bsd")
(setq c-basic-offset 4)
(setq css-indent-offset 2)
(setq js-indent-level 2)

(setq fill-column 79)

(setq python-fill-docstring-style 'django)
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))

(recentf-mode 1)
(setq recentf-exclude '(;;".*init\.el"
                        ;;".*\/my-.*\.el"
                        "^/var/folders\\.*"
                        "COMMIT_EDITMSG\\'"
                        ".*-autoloads\\.el\\'"
                        "[/\\]\\.elpa/"))
(setq recentf-max-menu-items 128)
(setq recentf-max-saved-items 256)

(setq scroll-step 4)
(setq scroll-margin 6)
(setq scroll-conservatively 8)
(setq scroll-preserve-screen-position t)

(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'fundamental-mode)

;; make indent commands use space only (never tab character)
;; emacs 23.1 to 26, default to t

;; If indent-tabs-mode is t, it means it may use tab, resulting mixed space and
;; tab
(setq-default indent-tabs-mode nil)

;; make tab key always call a indent command.
;; (setq-default tab-always-indent t)

;; make tab key call indent command or insert tab character, depending on cursor position
;; (setq-default tab-always-indent nil)

;; make tab key do indent first then completion.
(setq-default tab-always-indent 'complete)

(setq term-buffer-maximum-size 200000)

(setq tramp-backup-directory-alist backup-directory-alist)
(setq tramp-default-method "ssh")
(setf tramp-persistency-file-name (concat temporary-file-directory "tramp-" (user-login-name)))

;; (setq password-cache t) ; enable password caching
;; (setq password-cache-expiry 3600) ; for one hour (time in secs)

(setq undo-limit 150000)
(setq undo-strong-limit 300000)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

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

;; ansi-color support
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

(provide 'my-settings)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; my-settings.el ends here
