;;; my-org.el ---

;;; Commentary:

;; Copyright (C) 2019 
;; Author:  Toby Slight

;;; Code:
;; -*- lexical-binding: t; -*-

(require 'org)
(setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
(setq org-emphasis-regexp-components '(" \t('\"{" "- \t.,:!?;'\")}\\" " \t\r\n,\"'" "." 300))
(setq org-confirm-babel-evaluate t)
(setq org-agenda-files (file-expand-wildcards "~/*.org"))
(setq org-agenda-files (quote ("~/org/todo.org")))
(setq org-default-notes-file "~/org/notes.org")
(setq org-directory "~/org")
(setq org-export-with-toc t)
(setq org-completion-use-ido t)
(setq org-indent-indentation-per-level 1)
(setq org-list-allow-alphabetical t)
(setq org-list-indent-offset 1)
;; (setq org-replace-disputed-keys t) ;; fix windmove conflicts
(setq org-return-follows-link t)
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-targets '((nil :maxlevel . 9)))
(setq org-speed-commands-user (quote (("N" . org-down-element)
                                      ("P" . org-up-element))))
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-src-window-setup 'current-window)
(setq org-startup-indented t)
(setq org-use-fast-todo-selection t)
(setq org-use-speed-commands t)

(setq org-capture-templates
      '(("t" "TODO Entry"
         entry (file+headline "~/org/todo.org" "CURRENT")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal Entry"
         entry (file+datetree "~/org/journal.org" "JOURNAL")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("h" "Health Note"
         entry (file+headline "~/org/notes.org" "HEALTH")
         "* %?\n  %i\n  %a")
        ("m" "Misc Note"
         entry (file+headline "~/org/notes.org" "MISC")
         "* %?\n  %i\n  %a")
        ("M" "Mathematics Note"
         entry (file+headline "~/org/notes.org" "MATHEMATICS")
         "* %?\n  %i\n  %a")
        ("P" "Philosophy Note"
         entry (file+headline "~/org/notes.org" "PHILOSOPHY")
         "* %?\n  %i\n  %a")
        ("p" "Programming Note"
         entry (file+headline "~/org/notes.org" "PROGRAMMING")
         "* %?\n  %i\n  %a")
        ("s" "Sysadmin Note"
         entry (file+headline "~/org/notes.org" "SYSADMIN")
         "* %?\n  %i\n  %a")
        ("w" "Webadmin Note"
         entry (file+headline "~/org/notes.org" "WEBADMIN")
         "* %?\n  %i\n  %a")))

(add-to-list 'org-structure-template-alist
             '("C" "#+BEGIN_SRC c\n?\n#+END_SRC"
               "<src lang=\"c\">\n\n</src>"))
(add-to-list 'org-structure-template-alist
             '("cl" "#+BEGIN_SRC common-lisp\n?\n#+END_SRC"
               "<src lang=\"common-lisp\">\n\n</src>"))
(add-to-list 'org-structure-template-alist
             '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"
               "<src lang=\"emacs-lisp\">\n\n</src>"))
(add-to-list 'org-structure-template-alist
             '("j" "#+BEGIN_SRC java\n?\n#+END_SRC"
               "<src lang=\"java\">\n\n</src>"))
(add-to-list 'org-structure-template-alist
             '("py" "#+BEGIN_SRC python\n?\n#+END_SRC"
               "<src lang=\"python\">\n\n</src>"))
(add-to-list 'org-structure-template-alist
             '("sh" "#+BEGIN_SRC sh\n?\n#+END_SRC"
               "<src lang=\"sh\">\n\n</src>"))

(defun my/org-recursive-sort ()
  "Sort all entries in the current buffer, recursively."
  (interactive)
  (org-map-entries
   (lambda ()
     (condition-case x
         (org-sort-entries nil ?a)
       (user-error)))))

(defun my/org-tangle-block()
  "Only tangle the block at point."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-babel-tangle)))

(provide 'my-org)
;;; my-org.el ends here
