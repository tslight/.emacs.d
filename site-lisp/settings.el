;;; settings.el --- misc settings  -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs Configuration

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:

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

(setq-default fill-column 79)
(set-default 'truncate-lines t)
(add-hook 'text-mode-hook 'auto-fill-mode)

(setq backward-delete-char-untabify-method 'all)
(setq create-lockfiles nil) ;; prevent creation of .#myfile.ext
(setq require-final-newline t) ;; useful for crontab
(setq set-mark-command-repeat-pop t) ;; repeating C-SPC after popping, pops it

(with-eval-after-load 'electric
  (electric-indent-mode)
  (electric-pair-mode)
  (show-paren-mode 1)
  (message "Lazy loaded electric :-)"))

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

(setq custom-file (make-temp-file "emacs-custom"))
(setq disabled-command-function nil) ;; enable all "advanced" features
(setq message-log-max 10000)
(setq apropos-do-all t) ;; doesn't seem to be documented anywhere..

(setq mouse-yank-at-point t)

(setq scroll-step 4)
(setq scroll-margin 6)
(setq scroll-conservatively 8)
(setq scroll-preserve-screen-position t)

(defun display-startup-echo-area-message ()
  "Redefine this function to be more useful."
  (message "Started in %s. Hacks & Glory await! :-)" (emacs-init-time)))
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'fundamental-mode)

(with-eval-after-load 'tramp
  (setq tramp-backup-directory-alist backup-directory-alist)
  (setq tramp-default-method "ssh")
  (setf tramp-persistency-file-name (concat temporary-file-directory "tramp-" (user-login-name)))
  (message "Lazy loaded tramp :-)"))

;; (setq password-cache t) ; enable password caching
;; (setq password-cache-expiry 3600) ; for one hour (time in secs)

;; http://www.dr-qubit.org/Lost_undo-tree_history.html
(setq undo-limit 80000000)
(setq undo-strong-limit 90000000)

(setq user-full-name "Toby Slight")
(setq user-mail-address "tslight@pm.me")

(with-eval-after-load 'vc
  (setq vc-follow-symlinks t)
  (setq vc-make-backup-files t)
  (setq version-control t)
  (message "Lazy loaded vc :-)"))

(when (fboundp 'winner-mode) (winner-mode 1))
(setq split-width-threshold 160)
(setq split-height-threshold 80)

(fset 'yes-or-no-p 'y-or-n-p) ;; never have to type full word
(setq confirm-kill-emacs 'y-or-n-p)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; settings.el ends here
