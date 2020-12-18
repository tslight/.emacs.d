;;; use.el --- use-package configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Third Party Package configuration with the wonder of `use-package'.

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(require 'package)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-enable-imenu-support t
      ;; use-package-hook-name-suffix nil
      use-package-always-ensure t
      use-package-verbose t)
(require 'use-package)

(byte-recompile-file (concat user-emacs-directory "use.el") 'nil 0 'nil)

(use-package anaconda-mode :defer
  :hook
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode))

(use-package ansible :defer
  :hook (yaml-mode . ansible))

(use-package ansible-doc :defer
  :hook (yaml-mode . ansible-doc-mode))

(use-package async :defer
  :config (async-bytecomp-package-mode 1)
  :hook (dired-mode-hook . dired-async-mode))

(use-package blacken :defer
  :hook (python-mode . blacken-mode))

(use-package change-inner
  :bind
  ("M-i" . change-inner)
  ("M-o" . change-outer))

(use-package default-text-scale
  :if window-system
  :bind*
  ("C-M-=" . default-text-scale-increase)
  ("C-M--" . default-text-scale-decrease)
  ("C-M-0" . default-text-scale-reset))

(use-package diminish :defer 2
  :diminish abbrev-mode
  :diminish auto-fill-function ;; wtf?!
  :diminish eldoc-mode
  :diminish hs-minor-mode
  :diminish highlight-changes-mode
  :diminish my/key-mode
  :diminish org-indent-mode
  :diminish org-src-mode
  :diminish subword-mode
  :hook
  (org-indent-mode . (lambda () (diminish 'org-indent-mode)))
  (hs-minor-mode . (lambda () (diminish 'hs-minor-mode))))

(use-package docker
  :bind ("C-c C-d" . docker))

(use-package dockerfile-mode :defer)

(use-package exec-path-from-shell :defer 10
  :if (not (eq system-type 'windows-nt))
  :commands exec-path-from-shell-initialize
  :init
  (setq exec-path-from-shell-check-startup-files 'nil)
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PYTHONPATH"))

(use-package flycheck :defer
  :diminish flycheck-mode
  :hook (prog-mode . flycheck-mode)
  :config (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package git-timemachine :defer)

(use-package gitlab-ci-mode :defer
  :mode
  "\\.gitlab-ci.yaml\\'"
  "\\.gitlab-ci.yml\\'"
  :hook
  (yaml-mode . hs-minor-mode))

(use-package go-mode :defer
  :config
  (defun my/go-indent ()
    (setq indent-tabs-mode 1)
    (setq tab-width 2))
  :hook (go-mode . my/go-indent))

(use-package hungry-delete :defer 6
  :diminish hungry-delete-mode
  :config (global-hungry-delete-mode))

(use-package ibuffer-vc :defer
  :config
  (defun my/ibuffer-vc-setup ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                vc-relative-file)))
  :hook
  (ibuffer . my/ibuffer-vc-setup))

(use-package js2-mode :defer
  :hook
  (js-mode . js2-minor-mode)
  (js2-mode . js2-imenu-extras-mode)
  :mode
  "\\.js\\'")

(use-package js2-refactor :defer
  :hook (js2-mode . js2-refactor-mode)
  :bind (:map js2-mode-map
              ("C-k" . js2r-kill))
  :config (js2r-add-keybindings-with-prefix "C-c C-j"))

(use-package json-mode :defer
  :config
  (defun my/json-mode-setup ()
    (json-mode)
    (json-pretty-print (point-min) (point-max))
    (goto-char (point-min))
    (set-buffer-modified-p nil))
  (add-to-list 'auto-mode-alist
               '("\\.json\\'" . 'my/json-mode-setup)))

(use-package json-navigator :defer)

(use-package lazygitlab :ensure nil
  :if (file-directory-p "~/src/gitlab/tspub/lisp/lazygit")
  :load-path "~/src/gitlab/tspub/lisp/lazygit"
  :bind
  ("C-c g l a" . lazygitlab-clone-or-pull-all)
  ("C-c g l c" . lazygitlab-clone-or-pull-project)
  ("C-c g l g" . lazygitlab-clone-or-pull-group)
  ("C-c g l r" . lazygitlab-retriever)
  :config
  (defalias 'gl/api 'lazygitlab-retriever)
  (defalias 'gl/all 'lazygitlab-clone-or-pull-all)
  (defalias 'gl/grp 'lazygitlab-clone-or-pull-group)
  (defalias 'gl/repo 'lazygitlab-clone-or-pull-project))

(use-package lazygithub :ensure nil
  :if (file-directory-p "~/src/gitlab/tspub/lisp/lazygit")
  :load-path "~/src/gitlab/tspub/lisp/lazygit"
  :bind
  ("C-c g h a" . lazygithub-clone-or-pull-all)
  ("C-c g h c" . lazygithub-clone-or-pull-repo)
  ("C-c g h r" . lazygithub-retriever)
  :config
  (defalias 'gh/api 'lazygithub-retriever)
  (defalias 'gh/all 'lazygithub-clone-or-pull-all)
  (defalias 'gh/repo 'lazygithub-clone-or-pull-repo))

(use-package magit
  :bind*
  ("C-x g" . magit-status)
  :config
  (when (eq system-type 'windows-nt)
    (if (file-readable-p "C:/Program Files/Git/bin/git.exe")
        (setq magit-git-executable "C:/Program Files/Git/bin/git.exe"))
    (when (file-directory-p "C:/Program Files/Git/bin")
      (setq exec-path (add-to-list 'exec-path "C:/Program Files/Git/bin"))
      (setenv "PATH" (concat "C:\\Program Files\\Git\\bin;" (getenv "PATH")))))
  (setq magit-clone-set-remote.pushDefault t)
  (setq magit-completing-read-function 'magit-builtin-completing-read))

(use-package magit-repos :ensure nil
  :bind* ("C-x C-g" . magit-list-repositories)
  :config
  (setq magit-repository-directories `(("~/" . 0)
                                       ("~/src/gitlab" . 10)))
  (setq magit-repolist-columns
        '(("Name" 25 magit-repolist-column-ident)
          ;; ("Version" 25 magit-repolist-column-version)
          ("Pull" 5 magit-repolist-column-unpulled-from-upstream)
          ("Push" 5 magit-repolist-column-unpushed-to-upstream)
          ("Commit" 8 magit-repolist-column-flag t)
          ("Path" 99 magit-repolist-column-path))))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package nodejs-repl :defer
  :bind
  (:map js2-mode-map
        ("C-x C-e" . nodejs-repl-send-last-expression)
        ("C-c C-j" . nodejs-repl-send-line)
        ("C-c SPC" . nodejs-repl-send-region)
        ("C-c C-b" . nodejs-repl-send-buffer)
        ("C-c C-f" . nodejs-repl-load-file)
        ("C-c C-z" . nodejs-repl-switch-to-repl)))

(use-package org-bullets :defer
  :if window-system :hook (org-mode . org-bullets-mode))

(use-package htmlize :defer)

(use-package toc-org :defer
  :hook (org-mode . toc-org-enable))

(use-package pdf-tools :defer)

(use-package powershell
  :mode (("\\.ps1\\'" . powershell-mode)))

(use-package powerline :defer 4
  :config
  (if window-system ; use-package if doesn't work for emacsclient
      (powerline-default-theme)))

(use-package projectile :diminish projectile-mode
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode)
  ;; (setq projectile-completion-system 'ivy)
  (when (require 'magit nil t)
    (mapc #'projectile-add-known-project
          (mapcar #'file-name-as-directory (magit-list-repos)))
    ;; Optionally write to persistent `projectile-known-projects-file'
    (projectile-save-known-projects)))

(use-package restclient :defer)

(use-package systemd :defer)

(use-package terraform-mode :defer)

(use-package web-mode
  :mode
  "\\.phtml\\'"
  "\\.tpl\\.php\\'"
  "\\.[agj]sp\\'"
  "\\.as[cp]x\\'"
  "\\.erb\\'"
  "\\.mustache\\'"
  "\\.djhtml\\'"
  "\\.html\\.twig\\'"
  "\\.html?\\'"
  "\\.php?\\'"
  "\\.css?\\'"
  :hook
  (web-mode . js2-minor-mode)
  :config
  (setq web-mode-content-type "jsx")
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-block-face t)
  (setq web-mode-enable-part-face t)
  (setq web-mode-enable-comment-keywords t)
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil)))

(use-package wgrep :defer :commands wgrep
  :bind (:map grep-mode-map
              ("e" . wgrep-change-to-wgrep-mode)
              ("C-x C-q" . wgrep-change-to-wgrep-mode)))

(use-package which-key :defer 5
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package yaml-mode :defer)

(use-package yasnippet :defer
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets :defer)

(use-package yasnippet-classic-snippets :defer)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; use.el ends here
