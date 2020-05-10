;;; my-registers.el --- registers -*- lexical-binding: t; -*-

;;; Commentary:

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight tslight@pm.me
;; Version: 0.0.1
;; Package-Requires: nil
;; URL: https://github.com/tslight/.emacs.d

;;; Code:
(set-register ?h (cons 'file "~/"))
(set-register ?s (cons 'file "~/src/"))
(set-register ?j (cons 'file "~/src/gitlab/tspub/"))
(set-register ?a (cons 'file "~/src/gitlab/tspub/etc/agnostic"))
(set-register ?e (cons 'file "~/src/gitlab/tspub/etc/emacs/"))
(set-register ?l (cons 'file "~/src/gitlab/tspub/etc/emacs/site-lisp"))
(set-register ?o (cons 'file "~/src/gitlab/tsprv/org/"))
(set-register ?n (cons 'file "~/src/gitlab/tsprv/org/work/notes.org"))
(set-register ?t (cons 'file "~/src/gitlab/tsprv/org/work/todo.org"))
(set-register ?w (cons 'file "~/src/oe-developers/"))
(set-register ?b (cons 'file "~/src/oe-developers/be/"))
(set-register ?d (cons 'file "~/src/oe-developers/be/devops"))

(provide 'my-registers)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; my-registers.el ends here
