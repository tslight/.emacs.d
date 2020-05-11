;;; my-term.el --- term settings '  -*- lexical-binding: t; -*-

;;; Commentary:

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight tslight@pm.me
;; Version: 0.0.1
;; Package-Requires: nil
;; URL: https://github.com/tslight/.emacs.d

;;; Code:
(defun my/switch-to-ansi-term ()
  "Open an `ansi-term' if it doesn't already exist.
Otherwise switch to current one."
  (interactive)
  (if (get-buffer "*ansi-term*")
      (switch-to-buffer "*ansi-term*")
    (ansi-term (getenv "SHELL"))))

(defun my/tramp-term (&optional path name)
  "Open an `ansi-term' at PATH.

If no PATH is given, use the value of `default-directory'.  PATH
may be a tramp remote path.  The `ansi-term' buffer is named based
on NAME."
  (interactive)
  (unless path (setq path default-directory))
  (unless name (setq name "ansi-term"))
  (ansi-term "/bin/bash" name)
  (let ((path (replace-regexp-in-string "^file:" "" path))
        (cd-str
         "fn=%s; if test ! -d $fn; then fn=$(dirname $fn); fi; cd $fn;")
        (bufname (concat "*" name "*" )))
    (if (tramp-tramp-file-p path)
        (let ((tstruct (tramp-dissect-file-name path)))
          (cond
           ((equal (tramp-file-name-method tstruct) "ssh")
            (process-send-string bufname (format
                                          (concat  "ssh -t %s '"
                                                   cd-str
                                                   "exec bash'; exec bash; clear\n")
                                          (tramp-file-name-host tstruct)
                                          (tramp-file-name-localname tstruct))))
           (t (error "Not implemented for method %s"
                     (tramp-file-name-method tstruct)))))
      (process-send-string bufname (format (concat cd-str " exec bash;clear\n")
                                           path)))))

;; get unicode characters in ansi-term - https://stackoverflow.com/a/7442266
;; (defadvice ansi-term (after advise-ansi-term-coding-system)
;;   "Get unicode characters in `ansi-term'."
;;   (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
;; (ad-activate 'ansi-term)

(defadvice term-handle-exit (after term-kill-buffer-on-exit activate)
  "Kill term when shell exits."
  (kill-buffer))

(setq term-buffer-maximum-size 200000)

(provide 'my-term)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; my-term.el ends here
