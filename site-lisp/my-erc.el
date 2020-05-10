;;; my-erc.el --- erc settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight tslight@pm.me
;; Version: 0.0.1
;; Package-Requires: nil
;; URL: https://github.com/tslight/.emacs.d

;;; Code:
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

(provide 'my-erc)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; my-erc.el ends here
