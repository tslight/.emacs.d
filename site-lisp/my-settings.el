;;; my-settings.el ---

;;; Commentary:

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight tslight@pm.me

;;; Code:
;; -*- lexical-binding: t; -*-
(setq abbrev-file-name (concat user-emacs-directory "abbrevs"))
(setq save-abbrevs 'silently)

(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
(setq auto-save-timeout 5)

(setq auto-window-vscroll nil)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t) ;; copy files, don't rename them.
(setq delete-old-versions t)
(setq kept-new-versions 12)
(setq kept-old-versions 12)

(setq ring-bell-function 'ignore)
(setq visible-bell 1)

(setq mouse-yank-at-point t)

(setq select-enable-clipboard t)
(setq select-enable-primary t)
(setq x-select-enable-clipboard-manager nil)
(setq save-interprogram-paste-before-kill t)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-diff-options "-w")

(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

(defun disable-y-or-n-p (orig-fun &rest args)
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
    (apply orig-fun args)))

(advice-add 'ediff-quit :around #'disable-y-or-n-p)

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

(setq erc-autojoin-channels-alist '(("freenode.net"
				     "#org-mode"
				     "#emacs")))
(setq erc-fill-column 80)
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
(setq erc-input-line-position -2)
(setq erc-keywords '("not2b"))
(setq erc-nick "not2b")
(setq erc-prompt-for-password t)
(setq erc-track-enable-keybindings t)

(require 'em-smart)
(setq eshell-history-size 2048)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)
(define-key my/keymap (kbd "C-c e s") 'eshell)

;; (require 'nnir)
(setq gnus-init-file "~/.emacs.d/init.el")
(setq gnus-home-directory "~/.emacs.d/")
(setq gnus-completing-read-function 'gnus-ido-completing-read)
(setq message-directory "~/.emacs.d/mail")
(setq gnus-directory "~/.emacs.d/news")
(setq nnfolder-directory "~/.emacs.d/mail/archive")
(setq gnus-use-full-window nil)
(setq gnus-select-method '(nntp "news.gnus.org"))
(setq gnus-summary-thread-gathering-function
      'gnus-gather-threads-by-subject)
(setq gnus-thread-hide-subtree t)
(setq gnus-thread-ignore-subject t)

(global-highlight-changes-mode)
(setq highlight-changes-visibility-initial-state nil)

(setq load-prefer-newer t) ;; if init.elc is older, use newer init.el

(setq display-line-numbers 'relative)

(setq epa-file-cache-passphrase-for-symmetric-encryption t)
(setf epa-pinentry-mode 'loopback)

(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(setq coding-system-for-read 'utf-8-unix)
(setq coding-system-for-write 'utf-8-unix)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

(setq compilation-scroll-output 'first-error)
(setq custom-file (make-temp-file "emacs-custom"))
(setq disabled-command-function nil) ;; enable all "advanced" features
(setq message-log-max 10000)
(setq apropos-do-all t) ;; doesn't seem to be documented anywhere..

(setq user-full-name "Toby Slight")
(setq user-mail-address "tslight@pm.me")

(setq doc-view-continuous t)
(setq doc-view-resolution 300)

(setq c-default-style "bsd")
(setq c-basic-offset 4)
(setq css-indent-offset 2)
(setq js-indent-level 2)

(setq python-fill-docstring-style 'django)
(setq fill-column 78)

(recentf-mode 1)
(setq recentf-exclude '(".*init\.el"
			".*\/my-.*\.el"
			"^/var/folders\\.*"
			"COMMIT_EDITMSG\\'"
			".*-autoloads\\.el\\'"
			"[/\\]\\.elpa/"))
(setq recentf-max-menu-items 128)
(setq recentf-max-saved-items 256)

(set-register ?h (cons 'file "~/"))
(set-register ?s (cons 'file "~/src/"))
(set-register ?j (cons 'file "~/src/tspub/"))
(set-register ?a (cons 'file "~/src/tspub/etc/agnostic"))
(set-register ?e (cons 'file "~/src/tspub/etc/emacs/"))
(set-register ?l (cons 'file "~/src/tspub/etc/emacs/site-lisp"))
(set-register ?o (cons 'file "~/src/tsprv/org/"))
(set-register ?n (cons 'file "~/src/tsprv/org/work/notes.org"))
(set-register ?t (cons 'file "~/src/tsprv/org/work/todo.org"))
(set-register ?w (cons 'file "~/src/oe-developers/"))
(set-register ?b (cons 'file "~/src/oe-developers/be/"))
(set-register ?d (cons 'file "~/src/oe-developers/be/devops"))

(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(setq savehist-save-minibuffer-history 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq bookmark-save-flag 1) ;; always save bookmarks to file
(save-place-mode 1)
(setq save-place-file (concat user-emacs-directory "saveplace.el"))

(setq scroll-step 4)
(setq scroll-margin 6)
(setq scroll-conservatively 8)

(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'fundamental-mode)

(setq term-buffer-maximum-size 200000)

(setq tramp-backup-directory-alist backup-directory-alist)
(setq tramp-default-method "ssh")
(setf tramp-persistency-file-name (concat temporary-file-directory "tramp-" (user-login-name)))

(setq undo-limit 150000)
(setq undo-strong-limit 300000)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(setq vc-follow-symlinks t)
(setq vc-make-backup-files t)
(setq version-control t)

(fset 'yes-or-no-p 'y-or-n-p) ;; never have to type full word
(setq confirm-kill-emacs 'y-or-n-p)

(when (fboundp 'winner-mode) (winner-mode 1))
(setq split-width-threshold 160)
(setq split-height-threshold 80)

(provide 'my-settings)
;;; my-settings.el ends here
