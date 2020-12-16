;;; registers.el --- registers -*- lexical-binding: t; -*-

;;; Commentary:

;; Set registers here

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

(global-set-key (kbd "C-x j") 'jump-to-register)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; registers.el ends here
