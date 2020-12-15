;;; my-use.el --- site-lisp configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; site-lisp directory package configuration
;; :bind*, :bind-keymap, :bind-keymap*, :mode, :interpreter & :hook all imply
;; :defer

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(use-package my-change-pairs
  :bind*
  ("C-c M-p" . my/change-pairs))

(use-package my-edits
  :commands my/untabify-buffer my/xml-pretty-print
  :bind*
  ("C-c d" . my/delete-inside)
  ("C-c u" . my/underline-text)
  ("M-s M-s" . my/surround)
  ("C-o" . my/open-line-above)
  ("C-S-o" . my/open-line-below)
  ("C-M-y" . my/yank-pop-forwards))

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

(use-package my-files
  :bind*
  ("C-c f d" . my/delete-this-file)
  ("C-c f c" . my/copy-file-name-to-clipboard)
  ("C-c f b" . my/make-backup-and-save)
  ("C-c f r" . my/rename-this-file-and-buffer)
  ("C-c f s" . my/sudoedit))

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
