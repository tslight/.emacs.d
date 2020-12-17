;;; imenu.el --- imenu stuff -*- lexical-binding: t; -*-

;;; Commentary:

;; imenu stuff

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(with-eval-after-load 'imenu
  ;; (defun try-to-add-imenu ()
  ;;   (condition-case nil (imenu-add-to-menubar "Imenu") (error nil)))
  ;; (add-hook 'font-lock-mode-hook 'try-to-add-imenu)
  (setq imenu-auto-rescan t)
  (message "Lazy loaded imenu :-)"))

(autoload 'imenu "imenu" nil t)
(global-set-key (kbd "C-c i") 'imenu)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; imenu.el ends here
