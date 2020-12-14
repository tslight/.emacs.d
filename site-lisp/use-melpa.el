;;; use.el --- 3rd party packages  -*- lexical-binding: t; -*-

;;; Commentary:

;; Configuration of 3rd party packages with `use-package'.
;; :bind*, :bind-keymap, :bind-keymap*, :mode, :interpreter & :hook all imply
;; :defer

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(use-package anaconda-mode
  :ensure t
  :after python-mode
  :hook
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode))

(use-package ansible
  :ensure t
  :defer t
  :config
  (add-hook 'yaml-mode-hook '(lambda () (ansible 1))))

(use-package ansible-doc
  :ensure t
  :defer t
  :config
  (add-hook 'yaml-mode-hook #'ansible-doc-mode))

(use-package async
  :ensure t
  :defer t
  :config
  (async-bytecomp-package-mode 1))

(use-package blacken
  :ensure t
  :hook
  (python-mode . blacken-mode))

(use-package change-inner
  :ensure t
  :bind
  ("M-i" . change-inner)
  ("M-o" . change-outer))

(use-package default-text-scale
  :if window-system
  :ensure t
  :bind*
  ("C-M-=" . default-text-scale-increase)
  ("C-M--" . default-text-scale-decrease)
  ("C-M-0" . default-text-scale-reset))

(use-package diminish
  :ensure t
  :defer 2
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
  :ensure t
  :bind ("C-c C-d" . docker))

(use-package dockerfile-mode
  :ensure t
  :defer t )

(use-package exec-path-from-shell
  :if (not (eq system-type 'windows-nt))
  :ensure t
  :commands exec-path-from-shell-initialize
  :defer 10
  :init
  (setq exec-path-from-shell-check-startup-files 'nil)
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PYTHONPATH"))

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
  "\\.gitlab-ci.yml\\'"
  :hook
  (yaml-mode . hs-minor-mode)
  (gitlab-ci-mode . (lambda () (setq display-line-numbers 'relative))))

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

(use-package icomplete-vertical
  :ensure t
  :demand
  :after icomplete
  :bind
  ("C-c y" . my/icomplete-kill-ring)
  :config
  (defun my/icomplete-kill-ring ()
    "Insert item from kill-ring, selected with completion."
    (interactive)
    (icomplete-vertical-do (:separator 'dotted-line :height 20)
      (insert (completing-read "Yank: " kill-ring nil t))))
  (icomplete-vertical-mode))

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
  (:map js2-mode-map
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

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
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

(use-package org-bullets
  :defer t
  :if window-system
  :ensure t
  :hook
  (org-mode . org-bullets-mode))

;; source code syntax highlighting when html exporting
(use-package htmlize
  :defer t
  :ensure t)

(use-package toc-org
  :defer t
  :ensure t
  :hook
  (org-mode . toc-org-enable))

(use-package ox-latex
  :defer t
  :config
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-listings 'minted)
  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

(use-package pdf-tools
  :ensure t
  :defer t)

(use-package powershell
  :ensure t
  :mode (("\\.ps1\\'" . powershell-mode)))

(use-package powerline
  :ensure t
  :defer 4
  :config
  (if window-system
      (powerline-default-theme)))

(use-package projectile
  :ensure t
  :diminish projectile-mode
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

(use-package restclient
  :ensure t
  :defer t)

(use-package smartparens
  :ensure t
  :diminish t
  :hook
  (common-lisp-mode . smartparens-mode)
  (emacs-lisp-mode . smartparens-mode)
  (lisp-mode . smartparens-mode)
  :config
  (sp-use-paredit-bindings))

(use-package systemd
  :ensure t
  :defer t)

(use-package terraform-mode
  :ensure t
  :defer t)

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
  :diminish which-key-mode
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
  :defer t)

(use-package yasnippet-classic-snippets
  :ensure t
  :defer t)

(provide 'use-melpa)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; use-melpa.el ends here
