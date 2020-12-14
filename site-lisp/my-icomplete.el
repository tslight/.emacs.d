;;; my-icomplete.el --- icomplete functions -*- lexical-binding: t; -*-

;;; Commentary:

;; Extend icomplete

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(defun my/icomplete-recentf ()
  "Show a list of recent files."
  (interactive)
  (let* ((all-files recentf-list)
         (list1 (mapcar (lambda (x) (file-name-nondirectory x) x) all-files))
         (list2 (mapcar #'substring-no-properties list1))
         (list3 (mapcar #'abbreviate-file-name list2))
         (list4 (cl-remove-duplicates list3 :test #'string-equal)))
    (find-file (completing-read "Recent Files: " list4 nil t))))

(defun my/icomplete-styles ()
  "Set icomplete styles based on Emacs version."
  (if (version< emacs-version "27")
      (setq-local completion-styles '(initials partial-completion substring basic))
    (setq-local completion-styles '(initials partial-completion flex substring basic))))

(provide 'my-icomplete)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; my-icomplete.el ends here
