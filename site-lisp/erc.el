;;; erc.el --- erc configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; ERC configuration

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(with-eval-after-load 'erc
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

  (message "Lazy loaded erc :-)"))

(autoload 'erc "erc" nil t)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; erc.el ends here
