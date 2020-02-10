;;; use.el ---

;;; Commentary:

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight tslight@pm.me

;;; Code:
;; -*- lexical-binding: t; -*-
(use-package ace-window
  :ensure t
  :bind*
  ("C-x o" . ace-window)
  ("C-x 0" . ace-delete-window)
  ("C-x 1" . ace-delete-other-windows)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  ;; (setq aw-background nil)
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:foreground
	  "green"
	  ;; :background
	  ;; "black"
	  :height
	  2.0))))))

(use-package anaconda-mode
  :ensure t
  :after python-mode
  :hook
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode))

(use-package ansible
  :ensure t
  :defer t
  :mode
  "\\-pb.yaml\\'"
  "\\-pb.yml\\'"
  "\\.pb.yaml\\'"
  "\\.pb.yml\\'")

(use-package ansible-doc
  :ensure t
  :after ansible)

(use-package async
  :ensure t
  :defer t
  :config
  (async-bytecomp-package-mode 1))

(use-package browse-kill-ring
  :ensure t
  :bind*
  ("M-y" . browse-kill-ring))

(use-package change-inner
  :ensure t
  :bind
  ("M-i" . change-inner)
  ("M-o" . change-outer))

(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  :hook
  (prog-mode . company-mode))

(use-package company-anaconda
  :ensure t
  :after (:all company-mode anaconda-mode))

(use-package company-go
  :ensure t
  :after company-mode go-mode)

(use-package company-terraform
  :ensure t
  :after (:all company-mode terraform-mode))

(use-package diminish
  :ensure t
  :defer 4)

(use-package docker
  :ensure t
  :bind ("C-c C-d" . docker))

(use-package dockerfile-mode
  :ensure t
  :defer t )

(use-package dot-mode
  :ensure t
  :config
  (global-dot-mode))

(use-package exec-path-from-shell
  :ensure t
  :defer t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PYTHONPATH"))

(use-package flx-ido
  :ensure t
  :config
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t)
  ;; disable ido faces to see flx highlights.
  (setq ido-use-faces nil))

(use-package flycheck
  :ensure t
  :defer t
  :diminish flycheck-mode
  :hook
  (prog-mode . flycheck-mode)
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package git-timemachine
  :ensure t
  :defer t)

(use-package gitlab-ci-mode
  :ensure t
  :defer t
  :mode
  "\\.gitlab-ci.yaml\\'"
  "\\.gitlab-ci.yml\\'")

(use-package go-mode
  :ensure t
  :defer t
  :config
  (add-hook 'go-mode-hook
	    (lambda ()
	      (setq indent-tabs-mode 1)
	      (setq tab-width 2))))

(use-package hungry-delete
  :ensure t
  :defer 6
  :diminish hungry-delete-mode
  :config
  (global-hungry-delete-mode))

(use-package ibuffer-vc
  :ensure t
  :defer t
  :init
  (add-hook 'ibuffer-hook
	    (lambda ()
	      (ibuffer-vc-set-filter-groups-by-vc-root)
	      (unless (eq ibuffer-sorting-mode 'alphabetic)
		(ibuffer-do-sort-by-alphabetic))))
  :config
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
		vc-relative-file))))

(use-package iedit
  :ensure t
  :bind
  ("M-%" . iedit-mode))

(use-package js2-mode
  :ensure t
  :hook
  (js-mode . js2-minor-mode)
  (js2-mode . js2-imenu-extras-mode)
  :mode
  "\\.js\\'")

(use-package js2-refactor
  :ensure t
  :hook
  (js2-mode . js2-refactor-mode)
  :bind
  (:map
   js2-mode-map
   ("C-k" . js2r-kill))
  :config
  (js2r-add-keybindings-with-prefix "C-c C-j"))

(use-package json-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist
	       '("\\.json\\'" . (lambda ()
				  (json-mode)
				  (json-pretty-print (point-min) (point-max))
				  (goto-char (point-min))
				  (set-buffer-modified-p nil)))))

(use-package json-navigator
  :ensure t
  :defer t)

(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status)
  ("C-c g d" . magit-dispatch)
  ("C-c g l" . magit-list-repositories)
  :init
  (setq magit-repository-directories `(("~/" . 10)))
  (setq magit-repolist-columns
	'(("Name" 10 magit-repolist-column-ident nil)
	  ("Version" 25 magit-repolist-column-version nil)
	  ("Pull" 6 magit-repolist-column-unpulled-from-upstream)
	  ("Push" 6 magit-repolist-column-unpushed-to-upstream)
	  ("Commit" 8 magit-repolist-column-dirty)
	  ("Path" 99 magit-repolist-column-path nil)))
  :config
  (setq magit-completing-read-function 'magit-builtin-completing-read))

(use-package markdown-mode
  :ensure t
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package nix-mode
  :ensure t
  :defer t)

(use-package nodejs-repl
  :ensure t
  :defer t
  :bind
  (:map js2-mode-map
	("C-x C-e" . nodejs-repl-send-last-expression)
	("C-c C-j" . nodejs-repl-send-line)
	("C-c SPC" . nodejs-repl-send-region)
	("C-c C-b" . nodejs-repl-send-buffer)
	("C-c C-f" . nodejs-repl-load-file)
	("C-c C-z" . nodejs-repl-switch-to-repl)))

(use-package paredit
  :ensure t
  :bind
  ("C-c t p" . paredit-mode)
  ("C-c (" . paredit-forward-slurp-sexp)
  ("C-c )" . paredit-backward-slurp-sexp)
  ("C-c <" . paredit-forward-barf-sexp)
  ("C-c >" . paredit-backward-barf-sexp)
  ("C-c M-s" . paredit-splice-sexp)
  :hook
  (common-lisp-mode . paredit-mode)
  (emacs-lisp-mode . paredit-mode)
  (lisp-mode . paredit-mode)
  :config
  (unbind-key "M-s" paredit-mode-map))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ido)
  (when (require 'magit nil t)
    (mapc #'projectile-add-known-project
	  (mapcar #'file-name-as-directory (magit-list-repos)))
    ;; Optionally write to persistent `projectile-known-projects-file'
    (projectile-save-known-projects)))

(use-package py-autopep8
  :ensure t
  :after python-mode
  :hook
  (python-mode . py-autopep8-enable-on-save))

(use-package restclient
  :ensure t
  :defer t)

(use-package systemd
  :ensure t
  :defer t)

(use-package terraform-mode
  :ensure t
  :defer t)

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :bind
  ("C-x u" . undo-tree-visualize)
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist
	`(("." . ,(expand-file-name "~/.emacs.d/undos/")))))

(use-package web-mode
  :ensure t
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

(use-package wgrep
  :ensure t
  :defer t)

(use-package which-key
  :ensure t
  :defer 5
  :config
  (which-key-mode))

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package yasnippet
  :ensure t
  :defer t
  :diminish yas-minor-mode
  :hook
  (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t
  :defer t
  :after yasnippet)

(provide 'use)
;;; use.el ends here
