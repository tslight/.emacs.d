;;; compile.el --- compiling stuff -*- lexical-binding: t; -*-

;;; Commentary:

;; Extra elisp complilation stuff

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(defvar my/directories-to-recompile
  '("early-init.d" "init.d" "site-lisp" "use.d")
  "Directories under `user-emacs-directory' that we use for configuration.")

(defvar my/files-to-recompile
  '("early-init.el" "init.el" "use.el")
  "Directories under `user-emacs-directory' that we use for configuration.")

(defun my/recompile-site-lisp ()
  "Recompile everything in Emacs configuration."
  (interactive)
  (mapc (lambda (directory) (byte-recompile-directory (concat user-emacs-directory directory) 0 t))
        my/directories-to-recompile)
  (mapc (lambda (file) (byte-recompile-file (concat user-emacs-directory file) 0))
        my/files-to-recompile))

(defun my/auto-recompile ()
  "Automatically recompile Emacs Lisp files whenever they are saved."
  (when (equal major-mode 'emacs-lisp-mode)
    (progn
      (byte-compile-file buffer-file-name t)
      (message (concat "Re-compiled " buffer-file-name)))))

(setq compilation-scroll-output 'first-error)

(add-hook 'after-save-hook 'my/auto-recompile)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; compile.el ends here
