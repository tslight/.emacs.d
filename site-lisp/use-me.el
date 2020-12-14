;;; my-use.el --- site-lisp configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; site-lisp directory package configuration
;; :bind*, :bind-keymap, :bind-keymap*, :mode, :interpreter & :hook all imply
;; :defer

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:

;; Stuff that we simply must have when starting Emacs, everything else is
;; either implicitly or explicitly deferred.
(use-package my-registers :demand)
(use-package my-settings :demand)
(use-package my-style :demand)
(use-package my-theme :demand
  :bind*
  ("C-c M-t C-t" . my/disable-themes))

;; Everything declared under here should be deferred.
(use-package my-align
  :bind*
  ("C-c a" . my/align-symbol)
  ("C-c =" . my/align-equals)
  ("C-c :" . my/align-colon)
  ("C-c #" . my/align-numbers))

(use-package my-buffers
  :bind*
  ("C-c b b" . my/last-buffer)
  ("C-c b t" . my/toggle-buffer)
  ("C-<escape>" . my/last-buffer)
  ("C-M-<escape>" . my/toggle-buffer)
  ("C-c TAB" . my/indent-buffer)
  ("C-c b i" . my/indent-buffer)
  ("C-c z" . my/toggle-maximize-buffer)
  ("C-c M-n" . my/nuke-buffers)
  ("C-c s" . my/save-buffers-silently)
  ("C-x k" . my/kill-this-buffer)
  ("M-s s" . my/search-all-buffers)
  ("C-c M-t m" . (lambda () (interactive) (my/toggle-buffer "*Messages*")))
  ("C-c M-t s" . (lambda () (interactive) (my/toggle-buffer "*scratch*"))))

(use-package my-change-numbers
  :bind*
  ("C-c +" . my/increment-number-at-point)
  ("C-c -" . my/decrement-number-at-point))

(use-package my-change-pairs
  :bind*
  ("C-c M-p" . my/change-pairs))

(use-package my-dired
  :after dired
  :bind
  (:map dired-mode-map
        ("?" . my/dired-get-size)
        ("s" . my/dired-sort)
        ("C-RET" . my/dired-get-size)
        ("o" . my/dired-view-current)
        ([remap beginning-of-buffer] . my/dired-back-to-top)
        ([remap end-of-buffer] . my/dired-jump-to-bottom)))

(use-package my-edits
  :commands my/untabify-buffer my/xml-pretty-print
  :bind*
  ("C-x RET u" . my/convert-to-unix-coding-system)
  ("C-c d" . my/delete-inside)
  ("C-c u" . my/underline-text)
  ("M-s M-s" . my/surround)
  ("C-o" . my/open-line-above)
  ("C-S-o" . my/open-line-below)
  ("C-M-y" . my/yank-pop-forwards)
  :hook
  (after-save . my/auto-recompile)
  (find-file . my/hide-dos-eol))

(use-package my-mark-mitigations
  :commands
  my/exchange-point-and-mark-no-activate
  my/jump-to-mark
  my/push-mark-no-activate
  :bind
  ("C-c SPC p" . my/push-mark-no-activate)
  ("C-c SPC j" . my/jump-to-mark)
  ("C-c SPC x" . my/exchange-point-and-mark-no-activate)
  :hook
  (before-save . my/push-mark-no-activate))

(use-package my-eshell
  :bind*
  ("C-c C-e" . my/eshell-switcher)
  ("C-c M-e" . my/eshell-here)
  :bind
  (:map eshell-mode-map
        ("C-c r" . my/eshell-recent-dir)
        ("C-c D" . my/eshell-directory-children)
        ("M-r" . my/eshell-complete-history))
  :commands eshell my/eshell-here
  :config
  (use-package em-smart)
  :hook
  (eshell-preoutput-filter-functions . ansi-color-apply)
  (eshell-mode . (lambda () (eshell-smart-initialize))))

(use-package my-files
  :bind*
  ("C-c f d" . my/delete-this-file)
  ("C-c f c" . my/copy-file-name-to-clipboard)
  ("C-c f b" . my/make-backup-and-save)
  ("C-c f r" . my/rename-this-file-and-buffer)
  ("C-c f s" . my/sudoedit))

(use-package my-icomplete
  :demand
  :after icomplete
  :hook
  (icomplete-minibuffer-setup . my/icomplete-styles)
  :bind*
  ("C-c r" . my/icomplete-recentf))

(use-package my-isearch
  :bind
  (:map isearch-mode-map
        ("RET" . my/isearch-exit)
        ("C-w" . my/copy-to-isearch)
        ("M-w" . my/kill-to-isearch)))

(use-package my-misc
  :bind*
  ("C-c M-g" . my/google)
  ("C-c M-t l" . my/cycle-line-numbers)
  ("C-c Q c" . my/chuck-norris-joke)
  ("C-c Q k" . my/kanye-west-quote)
  ("C-c Q f" . my/fortune))

(use-package my-org
  :after org-mode
  :commands my/org-recursive-sort my/org-tangle-block)

(use-package my-removes
  :bind*
  ("C-c k" . my/remove-from-buffer))

(use-package my-smarter
  :bind*
  ([remap fill-paragraph] . smart/fill-or-unfill)
  ([remap move-beginning-of-line] . smart/move-beginning-of-line)
  ([remap kill-ring-save] . smart/kill-ring-save)
  ([remap kill-region] . smart/kill-region)
  :bind
  (:map ctl-x-map
        ("n" . smart/narrow-or-widen-dwim)))

(use-package my-term
  :bind*
  ("C-c t t" . my/switch-to-ansi-term)
  ("C-c t c" . (lambda () (interactive) (ansi-term (getenv "SHELL"))))
  :commands ansi-term term
  :hook
  (term-exec . (lambda () (set-process-coding-system 'utf-8-unix 'utf-8-unix))))

(use-package my-windows
  :bind*
  ("C-x O" . my/prev-window)
  ("C-c 3" . my/vsplit-last-buffer)
  ("C-c 2" . my/hsplit-last-buffer)
  ("C-M-<" . my/top-of-window)
  ("C-M->" . my/bottom-of-window)
  :bind
  ("M-p" . my/scroll-line-up)
  ("M-n" . my/scroll-line-down)
  (:map ctl-x-4-map
        ("k" . my/kill-buffer-other-window)
        ("o" . my/open-buffer-other-window)
        ("s" . my/toggle-split)
        ("t" . my/transpose-windows)))

(use-package my-wsl
  :if (string-match "-[Mm]icrosoft" operating-system-release)
  :bind*
  ("C-c C-w" . my/wsl-copy)
  ("C-c C-y" . my/wsl-yank))

(provide 'use-me)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; use-me.el ends here
