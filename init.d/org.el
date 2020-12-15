;;; org.el --- org mode custom functions  -*- lexical-binding: t; -*-

;;; Commentary:

;; custom org mode functions

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(with-eval-after-load 'org
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

  (setq org-image-actual-width nil)
  (setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  (setq org-emphasis-regexp-components '(" \t('\"{" "- \t.,:!?;'\")}\\" " \t\r\n,\"'" "." 300))
  (setq org-confirm-babel-evaluate t)
  (setq org-agenda-files (file-expand-wildcards "~/*.org"))
  (setq org-agenda-files (quote ("~/org/todo.org")))
  (setq org-default-notes-file "~/org/notes.org")
  (setq org-directory "~/org")
  (setq org-export-with-toc t)
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

  (add-to-list 'org-structure-template-alist '("cl" . "src common-lisp"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("go" . "src go"))
  (add-to-list 'org-structure-template-alist '("ja" . "src java"))
  (add-to-list 'org-structure-template-alist '("js" . "src javascript"))
  (add-to-list 'org-structure-template-alist '("kr" . "src c"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("sq" . "src sql"))
  (add-to-list 'org-structure-template-alist '("tx" . "src text"))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((awk . t)
     (C . t)
     (clojure . t)
     (css . t)
     (dot . t) ;; graphviz language
     (emacs-lisp . t)
     (gnuplot . t)
     (haskell . t)
     ;; (http . t)
     (java . t)
     (js . t)
     (latex . t)
     (lisp . t)
     (makefile . t)
     (ocaml . t)
     (perl . t)
     (python . t)
     (plantuml . t)
     (ruby . t)
     (scheme . t)
     (sed . t)
     (shell . t)
     (sql . t)
     (sqlite . t)))

  (global-set-key (kbd "C-c C-o a") 'org-agenda)
  (global-set-key (kbd "C-c C-o c") 'org-capture)
  (global-set-key (kbd "C-c C-o l") 'org-store-link)
  (global-set-key (kbd "C-c C-o t") 'org-time-stamp)

  (add-hook 'org-mode-hook 'auto-fill-mode)
  (add-hook 'org-mode-hook 'hl-line-mode))
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; org.el ends here
