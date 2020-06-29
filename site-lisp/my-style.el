;;; my-style.el --- style settings -*- lexical-binding: t; -*-

;;; Commentary:

;; code style settings

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(setq c-default-style "bsd")
(setq c-basic-offset 4)
(setq css-indent-offset 2)
(setq js-indent-level 2)

;; If indent-tabs-mode is t, it means it may use tab, resulting mixed space and
;; tab
(setq-default indent-tabs-mode nil)

;; make tab key always call a indent command.
;; (setq-default tab-always-indent t)

;; make tab key call indent command or insert tab character, depending on cursor position
;; (setq-default tab-always-indent nil)

;; make tab key do indent first then completion.
(setq-default tab-always-indent 'complete)

(provide 'my-style)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; my-style.el ends here
