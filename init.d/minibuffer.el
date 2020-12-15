;;; minibuffer.el --- minibuffer/completion configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Extend minibuffer and better default complete engine/settings

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(require 'recentf)
(recentf-mode 1)
(setq recentf-exclude '(;;".*init\.el"
                        ;;".*\/my-.*\.el"
                        "^/var/folders\\.*"
                        "COMMIT_EDITMSG\\'"
                        ".*-autoloads\\.el\\'"
                        "[/\\]\\.elpa/"))
(setq recentf-max-menu-items 128)
(setq recentf-max-saved-items 256)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(setq savehist-save-minibuffer-history 1)

(setq imenu-auto-rescan t)

(defun my/completing-recentf ()
  "Show a list of recent files."
  (interactive)
  (let* ((all-files recentf-list)
         (list1 (mapcar (lambda (x) (file-name-nondirectory x) x) all-files))
         (list2 (mapcar #'substring-no-properties list1))
         (list3 (mapcar #'abbreviate-file-name list2))
         (list4 (cl-remove-duplicates list3 :test #'string-equal)))
    (find-file (completing-read "Recent Files: " list4 nil t))))
(global-set-key (kbd "C-c r") 'my/completing-recentf)

(require 'my-fido-mode)
(my-fido-mode 1)

(add-hook 'minibuffer-exit-hook (lambda () (setq gc-cons-threshold 800000)))
(add-hook 'minibuffer-setup-hook (lambda () (setq gc-cons-threshold most-positive-fixnum)))
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; minibuffer.el ends here
