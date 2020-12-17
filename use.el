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

;;;; Anaconda
(use-package anaconda-mode :defer
  :hook
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode))

;;;; Ansible
(use-package ansible :defer
  :hook (yaml-mode . ansible))

;;;; Ansible Doc
(use-package ansible-doc :defer
  :hook (yaml-mode . ansible-doc-mode))

;;;; Async
(use-package async :defer
  :config (async-bytecomp-package-mode 1))

;;;; Blacken
(use-package blacken :defer
  :hook (python-mode . blacken-mode))

;;;; Change Inner
(use-package change-inner
  :bind
  ("M-i" . change-inner)
  ("M-o" . change-outer))

;;;; Default Text Scale
(use-package default-text-scale
  :if window-system
  :bind*
  ("C-M-=" . default-text-scale-increase)
  ("C-M--" . default-text-scale-decrease)
  ("C-M-0" . default-text-scale-reset))

;;;; Diminish
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

;;;; Docker
(use-package docker
  :bind ("C-c C-d" . docker))

;;;; Dockerfile
(use-package dockerfile-mode :defer)

;;;; Exec $PATH from Shell
(use-package exec-path-from-shell :defer 10
  :if (not (eq system-type 'windows-nt))
  :commands exec-path-from-shell-initialize
  :init
  (setq exec-path-from-shell-check-startup-files 'nil)
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PYTHONPATH"))

;;;; Flycheck
(use-package flycheck :defer
  :diminish flycheck-mode
  :hook (prog-mode . flycheck-mode)
  :config (flycheck-add-mode 'javascript-eslint 'web-mode))

;;;; Git Timemachine
(use-package git-timemachine :defer)

;;;; Gitlab CI
(use-package gitlab-ci-mode :defer
  :mode
  "\\.gitlab-ci.yaml\\'"
  "\\.gitlab-ci.yml\\'"
  :hook
  (yaml-mode . hs-minor-mode))

;;;; Go Lang
(use-package go-mode :defer
  :config
  (defun my/go-indent ()
    (setq indent-tabs-mode 1)
    (setq tab-width 2))
  :hook (go-mode . my/go-indent))

;;;; Hungry Delete
(use-package hungry-delete :defer 6
  :diminish hungry-delete-mode
  :config (global-hungry-delete-mode))

;;;; Ibuffer VC
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

;;;; JavaScript
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

;;;; JSON
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

;;;; Magit
(use-package magit
  :bind*
  ("C-x g" . magit-status)
  ("C-x C-g" . magit-dispatch)
  ("C-c C-g" . magit-list-repositories)
  :init
  (setq magit-repository-directories `(("~/src/gitlab" . 10)))
  (setq magit-repolist-columns
        '(("Name" 25 magit-repolist-column-ident)
          ;; ("Version" 25 magit-repolist-column-version)
          ("Pull" 5 magit-repolist-column-unpulled-from-upstream)
          ("Push" 5 magit-repolist-column-unpushed-to-upstream)
          ("Commit" 8 magit-repolist-column-dirty t)
          ("Path" 99 magit-repolist-column-path)))
  :config
  (setq magit-clone-set-remote.pushDefault t)
  (setq magit-completing-read-function 'magit-builtin-completing-read))

;;;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :init (setq markdown-command "multimarkdown"))

;;;; Node
(use-package nodejs-repl :defer
  :bind
  (:map js2-mode-map
        ("C-x C-e" . nodejs-repl-send-last-expression)
        ("C-c C-j" . nodejs-repl-send-line)
        ("C-c SPC" . nodejs-repl-send-region)
        ("C-c C-b" . nodejs-repl-send-buffer)
        ("C-c C-f" . nodejs-repl-load-file)
        ("C-c C-z" . nodejs-repl-switch-to-repl)))

;;;; Org
(use-package org-bullets :defer
  :if window-system :hook (org-mode . org-bullets-mode))

;; source code syntax highlighting when html exporting
(use-package htmlize :defer)

(use-package toc-org :defer
  :hook (org-mode . toc-org-enable))

;;;; PDF Tools
(use-package pdf-tools :defer)

;;;; Powershell
(use-package powershell
  :mode (("\\.ps1\\'" . powershell-mode)))

;;;; Powerline
(use-package powerline :defer 4
  :config
  (if window-system ; use-package if doesn't work for emacsclient
      (powerline-default-theme)))

;;;; Projectile
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

;;;; Restclient
(use-package restclient :defer)

;;;; systemd
(use-package systemd :defer)

;;;; Terraform
(use-package terraform-mode :defer)

;;;; Web
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

;;;; Wgrep
(use-package wgrep :defer :commands wgrep
  :bind (:map grep-mode-map
              ("e" . wgrep-change-to-wgrep-mode)
              ("C-x C-q" . wgrep-change-to-wgrep-mode)))

;;;; Which Key?
(use-package which-key :defer 5
  :diminish which-key-mode
  :config
  (which-key-mode))

;;;; YAML
(use-package yaml-mode :defer)

;;;; Yasnippet
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
