;;; recentf.el --- recent file config -*- lexical-binding: t; -*-

;;; Commentary:

;; recenf file bumpf

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(with-eval-after-load 'recentf
  (setq recentf-exclude '("^/var/folders\\.*"
                          "COMMIT_EDITMSG\\'"
                          ".*-autoloads\\.el\\'"
                          "[/\\]\\.elpa/"))
  (setq recentf-max-menu-items 128)
  (setq recentf-max-saved-items 256)

  (global-set-key (kbd "C-c C-r") 'recentf-open-files)

  ;;;###autoload
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

  (message "Lazy loaded recentf :-)"))

(recentf-mode 1)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; recentf.el ends here
