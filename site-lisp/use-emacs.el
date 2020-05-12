;;; use-emacs.el --- use package for emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; `use-package' configuration for built in Emacs stuff :commands, :bind,
;; :bind*, :bind-keymap, :bind-keymap*, :mode, :interpreter & :hook all imply
;; :defer

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(use-package ansi-color
  :config
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
  (defun colorize-compilation-buffer ()
    (ansi-color-apply-on-region compilation-filter-start (point)))
  :hook
  (compilation-filter . colorize-compilation-buffer)
  (shell-mode . ansi-color-for-comint-mode-on))

(use-package dired
  :after dired-x
  :bind*
  ("C-x C-d" . dired)
  ("C-x M-d" . list-directory)
  :bind
  (:map dired-mode-map
        (")" . dired-omit-mode)
        ("b" . (lambda () (interactive (find-alternate-file ".."))))
        ("f" . dired-find-alternate-file)
        ("c" . dired-do-compress-to)
        ("C-o" . dired-find-file-other-window))
  :config
  (when (eq system-type 'berkeley-unix)
    (progn
      (setq dired-listing-switches "-alhpL")))
  (setq dired-dwim-target t)
  (setq dired-use-ls-dired nil)
  ;; (setq dired-omit-mode t)
  ;; (setq-default dired-omit-files-p t) ; Buffer-local variable
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always))

(use-package dired-x
  :bind*
  ("C-x C-j" . dired-jump)
  ("C-x d" . dired-jump)
  :config
  (autoload 'dired-jump "dired-x" t))

(use-package find-dired
  :after dired
  :config
  (setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld")))

(use-package ediff
  :commands ediff ediff3
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-diff-options "-w")
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo)
  ;; https://emacs.stackexchange.com/a/24602
  (defun disable-y-or-n-p (orig-fun &rest args)
    "Advise ORIG-FUN with ARGS so it dynamically rebinds `y-or-n-p'."
    (cl-letf (((symbol-function 'y-or-n-p) (lambda () t)))
      (apply orig-fun args)))
  (advice-add 'ediff-quit :around #'disable-y-or-n-p))

(use-package eldoc
  :hook
  (emacs-lisp-mode . eldoc-mode)
  (lisp-interaction-mode . eldoc-mode)
  (lisp-mode . eldoc-mode))

(use-package electric
  :hook
  (prog-mode . electric-indent-mode)
  (prog-mode . electric-pair-mode)
  (shell-script-mode . electric-indent-mode)
  (shell-script-mode . electric-pair-mode))

(use-package erc
  :commands erc
  :config
  (setq erc-autojoin-channels-alist '(("freenode.net"
                                       "#org-mode"
                                       "#emacs")))
  (setq erc-fill-column 80)
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))
  (setq erc-input-line-position -2)
  (setq erc-keywords '("not2b"))
  (setq erc-nick "not2b")
  (setq erc-prompt-for-password t)
  (setq erc-track-enable-keybindings t))

(use-package gnus
  :commands gnus
  :config
  (require 'nnir)
  (setq gnus-init-file "~/.emacs.d/init.el")
  (setq gnus-home-directory "~/.emacs.d/")
  (setq message-directory "~/.emacs.d/mail")
  (setq gnus-directory "~/.emacs.d/news")
  (setq nnfolder-directory "~/.emacs.d/mail/archive")
  (setq gnus-use-full-window nil)
  (setq gnus-select-method '(nntp "news.gnus.org"))
  (setq gnus-summary-thread-gathering-function
        'gnus-gather-threads-by-subject)
  (setq gnus-thread-hide-subtree t)
  (setq gnus-thread-ignore-subject t))

(use-package hl-line
  :hook
  (dired-mode . hl-line-mode)
  (prog-mode . hl-line-mode)
  (shell-script-mode . hl-line-mode)
  (text-mode . hl-line-mode))

(use-package lisp-mode
  :hook
  (lisp-mode . (lambda () (add-hook 'after-save-hook 'check-parens nil t))))

(use-package prog-mode
  :hook
  (prog-mode . hs-minor-mode)
  (prog-mode . (lambda () (setq display-line-numbers 'relative))))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("ipython" . python-mode)
  :config
  (setq python-fill-docstring-style 'django))

(use-package recentf
  :defer 4
  :config
  (recentf-mode 1)
  (setq recentf-exclude '(;;".*init\.el"
                          ;;".*\/my-.*\.el"
                          "^/var/folders\\.*"
                          "COMMIT_EDITMSG\\'"
                          ".*-autoloads\\.el\\'"
                          "[/\\]\\.elpa/"))
  (setq recentf-max-menu-items 128)
  (setq recentf-max-saved-items 256))

(use-package savehist
  :defer 2
  :config
  (savehist-mode 1)
  (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (setq savehist-save-minibuffer-history 1))

(use-package shell-script-mode
  :mode
  "\\.sh\\'"
  "\\.bash\\'"
  "\\.zsh\\'"
  "\\bashrc\\'"
  "\\kshrc\\'"
  "\\profile\\'"
  "\\zshenv\\'"
  "\\zprompt\\'"
  "\\zshrc\\'"
  "^prompt_.*_setup$"
  :hook
  (shell-script-mode . (lambda () (setq display-line-numbers 'relative)))
  (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package text-mode
  :hook
  (text-mode . abbrev-mode)
  (text-mode . auto-fill-mode))

(use-package uniquify
  :defer 2
  :config
  (setq uniquify-buffer-name-style 'forward))

(provide 'use-emacs)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; use-emacs.el ends here
