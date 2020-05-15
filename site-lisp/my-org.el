;;; my-org.el --- org mode custom functions  -*- lexical-binding: t; -*-

;;; Commentary:

;; custom org mode functions

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(require 'org)
(require 'org-tempo)

(defun my/org-recursive-sort ()
  "Sort all entries in the current buffer, recursively."
  (interactive)
  (org-map-entries
   (lambda ()
     (condition-case x
         (org-sort-entries nil ?a)
       (user-error)))))

(defun my/org-tangle-block ()
  "Only tangle the block at point."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-babel-tangle)))

(provide 'my-org)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; my-org.el ends here
