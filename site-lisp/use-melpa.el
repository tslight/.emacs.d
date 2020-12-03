;;; use.el --- 3rd party packages  -*- lexical-binding: t; -*-

;;; Commentary:

;; Configuration of 3rd party packages with `use-package'.
;; :bind*, :bind-keymap, :bind-keymap*, :mode, :interpreter & :hook all imply
;; :defer

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
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

(use-package amx
  :ensure t)

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

(use-package change-inner
  :ensure t
  :bind
  ("M-i" . change-inner)
  ("M-o" . change-outer))

(use-package comment-dwim-2
  :ensure t
  :bind
  ("M-;" . comment-dwim-2)
  ("C-c ;" . comment-dwim-2)
  (:map org-mode-map
        ("M-;" . org-comment-dwim-2)
        ("C-c ;" . org-comment-dwim-2)))

(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  :hook
  (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 2
        company-idle-delay 0.2))

(use-package company-anaconda
  :ensure t
  :after (:all company-mode anaconda-mode))

(use-package company-ansible
  :ensure t
  :after (:all company-mode ansible))

(use-package company-go
  :ensure t
  :after company-mode go-mode)

(use-package company-terraform
  :ensure t
  :after (:all company-mode terraform-mode))

(use-package counsel
  :ensure t
  :diminish
  :bind*
  ("C-x C-f" . counsel-find-file)
  ("M-x" . counsel-M-x)
  ("M-y" . counsel-yank-pop)
  ("C-h a" . counsel-apropos)
  ("C-h f" . counsel-describe-function)
  ("C-h v" . counsel-describe-variable)
  ("C-h l" . counsel-load-library)
  ("C-h i" . counsel-info-lookup-symbol)
  ("C-h u" . counsel-unicode-char)
  ("C-c g c" . counsel-git)
  ("C-c g g" . counsel-git-grep)
  ("C-c M-g" . counsel-ag)
  ("C-c l" . counsel-locate)
  ("C-x r b" . counsel-bookmark)
  ("C-c f l" . counsel-find-library)
  ("C-c f f" . counsel-file-jump)
  ("C-c f j" . counsel-dired-jump)
  :config
  (counsel-mode 1))

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
  :ensure t
  :commands exec-path-from-shell-initialize
  :defer 10
  :init
  (setq exec-path-from-shell-check-startup-files 'nil)
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PYTHONPATH"))

(use-package flx
  :ensure t)

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

(use-package ivy
  :ensure t
  :diminish
  :bind*
  ("C-x b" . ivy-switch-buffer)
  ("C-M-r" . swiper)
  ("C-M-s" . swiper)
  ("C-S-s" . swiper-multi)
  ("C-c M-r" . ivy-resume)
  (:map ivy-minibuffer-map
        ("C-s" . ivy-next-line)
        ("C-r" . ivy-previous-line))
  :config
  (ivy-mode 1)
  (setq ivy-wrap t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 20)
  (setq ivy-initial-inputs-alist nil) ;; no ^
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t      . ivy--regex-fuzzy))))

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
  :if window-system
  :config
  ;; (setq powerline-default-separator 'bar)
  (powerline-default-theme))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy)
  (when (require 'magit nil t)
    (mapc #'projectile-add-known-project
          (mapcar #'file-name-as-directory (magit-list-repos)))
    ;; Optionally write to persistent `projectile-known-projects-file'
    (projectile-save-known-projects)))

(use-package blacken
  :ensure t
  :hook
  (python-mode . blacken-mode))

(use-package restclient
  :ensure t
  :defer t)

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

(provide 'use)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; use-melpa.el ends here
