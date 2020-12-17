;;; imenu.el --- imenu stuff -*- lexical-binding: t; -*-

;;; Commentary:

;; imenu stuff

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(with-eval-after-load 'imenu
  (setq imenu-auto-rescan t)
  (setq imenu-auto-rescan-maxout 600000)
  (setq imenu-eager-completion-buffer t)
  (setq imenu-level-separator "/")
  (setq imenu-max-item-length 100)
  (setq imenu-space-replacement " ")
  (setq imenu-use-markers t)
  (setq imenu-use-popup-menu nil)
  (message "Lazy loaded imenu :-)"))

(autoload 'imenu "imenu" nil t)
(global-set-key (kbd "C-c i") 'imenu)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; imenu.el ends here
