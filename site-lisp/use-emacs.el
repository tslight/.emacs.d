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
  :bind*
  ("C-x C-d" . dired)
  ("C-x C-j" . dired-jump)
  ("C-x d" . dired-jump)
  ("C-x M-d" . list-directory)
  (:map dired-mode-map
        (")" . dired-omit-mode)
        ("b" . (lambda () (interactive (find-alternate-file ".."))))
        ("f" . dired-find-alternate-file)
        ("c" . dired-do-compress-to)
        ("C-o" . dired-find-file-other-window))
  :init
  (require 'dired-x)
  :config
  (autoload 'dired-jump "dired-x" t)
  (when (eq system-type 'berkeley-unix)
    (progn
      (setq dired-listing-switches "-alhpL")))
  (setq dired-dwim-target t)
  (setq dired-use-ls-dired nil)
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always))

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
  (setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)
  (setq gnus-thread-hide-subtree t)
  (setq gnus-thread-ignore-subject t))

(use-package hl-line
  :hook
  (dired-mode . hl-line-mode)
  (prog-mode . hl-line-mode)
  (shell-script-mode . hl-line-mode)
  (text-mode . hl-line-mode))

;; (use-package ido
;;   :config
;;   (ido-mode 1)
;;   (ido-everywhere 1)
;;   (setq ido-use-virtual-buffer 't ;; show recent files too
;;         ido-create-new-buffer 'always
;;         ido-enable-prefix t
;;         ido-enable-flex-matching t
;;         ido-auto-merge-work-directories-length -1
;;         ido-use-filename-at-point 'ffap-guesser))

(use-package icomplete
  :hook
  (after-init . (lambda () (if (version< emacs-version "27")
                          (icomplete-mode)
                        (fido-mode))))
  :config
  (setq completion-styles '(flex
                            partial-completition
                            substring
                            initials
                            basic
                            emacs22))
  (setq icomplete-compute-delay 0)
  (setq icomplete-delay-completions-threshold 0)
  (setq icomplete-hide-common-prefix nil)
  (setq icomplete-in-buffer t)
  (setq icomplete-max-delay-chars 0)
  (setq icomplete-prospects-height 1)
  (setq icomplete-show-matches-on-no-input t))

(use-package lisp-mode
  :hook
  (lisp-mode . (lambda () (add-hook 'after-save-hook 'check-parens nil t))))

(use-package org
  :bind*
  ("C-c C-o a" . org-agenda)
  ("C-c C-o c" . org-capture)
  ("C-c C-o l" . org-store-link)
  ("C-c C-o t" . org-time-stamp)
  :hook
  (org-mode . auto-fill-mode)
  (org-mode . hl-line-mode)
  :config
  (setq org-image-actual-width nil)
  (setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  (setq org-emphasis-regexp-components '(" \t('\"{" "- \t.,:!?;'\")}\\" " \t\r\n,\"'" "." 300))
  (setq org-confirm-babel-evaluate t)
  (setq org-agenda-files (file-expand-wildcards "~/*.org"))
  (setq org-agenda-files (quote ("~/org/todo.org")))
  (setq org-default-notes-file "~/org/notes.org")
  (setq org-directory "~/org")
  (setq org-export-with-toc t)
  (setq org-indent-indentation-per-level 1)
  (setq org-list-allow-alphabetical t)
  (setq org-list-indent-offset 1)
  ;; (setq org-replace-disputed-keys t) ;; fix windmove conflicts
  (setq org-return-follows-link t)
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets '((nil :maxlevel . 9)))
  (setq org-speed-commands-user (quote (("N" . org-down-element)
                                        ("P" . org-up-element))))
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-window-setup 'current-window)
  (setq org-startup-indented t)
  (setq org-use-fast-todo-selection t)
  (setq org-use-speed-commands t)

  (setq org-capture-templates
        '(("t" "TODO Entry"
           entry (file+headline "~/org/todo.org" "CURRENT")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal Entry"
           entry (file+datetree "~/org/journal.org" "JOURNAL")
           "* %?\nEntered on %U\n  %i\n  %a")
          ("h" "Health Note"
           entry (file+headline "~/org/notes.org" "HEALTH")
           "* %?\n  %i\n  %a")
          ("m" "Misc Note"
           entry (file+headline "~/org/notes.org" "MISC")
           "* %?\n  %i\n  %a")
          ("M" "Mathematics Note"
           entry (file+headline "~/org/notes.org" "MATHEMATICS")
           "* %?\n  %i\n  %a")
          ("P" "Philosophy Note"
           entry (file+headline "~/org/notes.org" "PHILOSOPHY")
           "* %?\n  %i\n  %a")
          ("p" "Programming Note"
           entry (file+headline "~/org/notes.org" "PROGRAMMING")
           "* %?\n  %i\n  %a")
          ("s" "Sysadmin Note"
           entry (file+headline "~/org/notes.org" "SYSADMIN")
           "* %?\n  %i\n  %a")
          ("w" "Webadmin Note"
           entry (file+headline "~/org/notes.org" "WEBADMIN")
           "* %?\n  %i\n  %a")))

  (add-to-list 'org-structure-template-alist '("cl" . "src common-lisp"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("go" . "src go"))
  (add-to-list 'org-structure-template-alist '("ja" . "src java"))
  (add-to-list 'org-structure-template-alist '("js" . "src javascript"))
  (add-to-list 'org-structure-template-alist '("kr" . "src c"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("sq" . "src sql"))
  (add-to-list 'org-structure-template-alist '("tx" . "src text"))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((awk . t)
     (C . t)
     (clojure . t)
     (css . t)
     (dot . t) ;; graphviz language
     (emacs-lisp . t)
     (gnuplot . t)
     (haskell . t)
     ;; (http . t)
     (java . t)
     (js . t)
     (latex . t)
     (lisp . t)
     (makefile . t)
     (ocaml . t)
     (perl . t)
     (python . t)
     (plantuml . t)
     (ruby . t)
     (scheme . t)
     (sed . t)
     (shell . t)
     (sql . t)
     (sqlite . t))))

(use-package prog-mode
  :hook
  (prog-mode . hs-minor-mode)
  (prog-mode . (lambda () (setq display-line-numbers 'relative))))

(use-package yaml-mode
  :hook
  (yaml-mode . hs-minor-mode)
  (yaml-mode . (lambda () (setq display-line-numbers 'relative))))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter
  ("python" . python-mode)
  ("ipython" . python-mode)
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
  "\\.bash.*\\'"
  "\\.zsh.*\\'"
  "\\bashrc\\'"
  "\\kshrc\\'"
  "\\profile\\'"
  "\\zshenv\\'"
  "\\zprompt\\'"
  "\\zshrc\\'"
  "\\prompt_.*_setup\\'"
  :interpreter
  "bash"
  "ksh"
  "sh"
  "zsh"
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

(use-package whitespace
  :demand t
  :diminish
  :commands (whitespace-buffer
             whitespace-cleanup
             whitespace-mode)
  :bind
  ("C-c w w" . whitespace-mode)
  ("C-c w c" . whitespace-cleanup)
  :hook
  (before-save . whitespace-cleanup)
  :config
  (setq whitespace-line-column 120)
  (setq whitespace-style '(face
                           tabs
                           spaces
                           trailing
                           lines
                           space-before-tab::space
                           newline
                           indentation::space
                           empty
                           space-after-tab::space
                           space-mark
                           tab-mark
                           newline-mark)
        whitespace-face 'whitespace-trailing))

(provide 'use-emacs)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; use-emacs.el ends here
