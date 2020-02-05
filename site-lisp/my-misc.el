;;; my-misc.el ---

;;; Commentary:

;; Copyright (C) 2019 
;; Author:  Toby Slight

;;; Code:
;; -*- lexical-binding: t; -*-

(defun my/substring (substring string)
  "Returns SUBSTRING of a STRING."
  (let ((regex (concat  ".*\\(" substring "\\).*")))
    (string-match regex string)
    (match-string 1 string)))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point)))

(defun my/switch-to-ansi-term ()
  "Open an ansi-term if it doesn't already exist, otherwise
  switch to current one."
  (interactive)
  (if (get-buffer "*ansi-term*")
      (switch-to-buffer "*ansi-term*")
    (ansi-term (getenv "SHELL"))))

(defun my/google (arg)
  "Googles a query or region.  With prefix ARG, wrap search query
  in quotes."
  (interactive "P")
  (let ((query
         (if (region-active-p)
             (buffer-substring (region-beginning) (region-end))
           (read-string "Query: "))))
    (when arg (setq query (concat "\"" query "\"")))
    (browse-url
     (concat "http://www.google.com/search?ie=utf-8&oe=utf-8&q=" query))))

(defun my/tramp-term (&optional path name)
  "Open an ansi terminal at PATH.  If no PATH is given, it uses
the value of `default-directory'.  PATH may be a tramp remote
path.  The ansi-term buffer is named based on NAME."
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

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(define-key my/keymap (kbd "C-c G") 'my/google)
(define-key my/keymap (kbd "C-x t") 'my/switch-to-ansi-term)

(provide 'my-misc)
;;; my-misc.el ends here
