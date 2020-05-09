;;; my-misc.el --- my-misc  -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs Configuration

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(defun my/substring (substring string)
  "Returns SUBSTRING of a STRING."
  (let ((regex (concat  ".*\\(" substring "\\).*")))
    (string-match regex string)
    (match-string 1 string)))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point)))

(defun my/cycle-line-numbers ()
  "Cycle through all the line numbering configurations."
  (interactive)
  (if display-line-numbers
      (if current-prefix-arg
          (if (eq display-line-numbers 'relative)
              (setq display-line-numbers t)
            (setq display-line-numbers 'relative))
        (setq display-line-numbers nil))
    (if current-prefix-arg
        (setq display-line-numbers 'relative)
      (setq display-line-numbers t))))

(defun my/switch-to-ansi-term ()
  "Open an ansi-term if it doesn't already exist, otherwise
  switch to current one."
  (interactive)
  (if (get-buffer "*ansi-term*")
      (switch-to-buffer "*ansi-term*")
    (ansi-term (getenv "SHELL"))))

(defun my/fortune ()
  "Insert a fortune into the minibuffer unless called with
`prefix-arg', in which case - insert output of the fortune
command into the buffer, before the point."
  (interactive)
  (if current-prefix-arg
      (insert (shell-command-to-string "fortune"))
    (message (string-trim (shell-command-to-string "fortune -s -n 100")))))

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

(defun my/kanye-west-quote ()
  "Get a random Kanye quote in the minibuffer."
  (interactive)
  (message
   (with-temp-buffer
     (url-insert-file-contents "https://api.kanye.rest/")
     (cdr (assoc 'quote (json-read))))))

(defun my/chuck-norris-joke ()
  "Get a random Chuck Norris joke in the minibuffer."
  (interactive)
  (message
   (with-temp-buffer
     (url-insert-file-contents "https://api.chucknorris.io/jokes/random")
     (cdr (assoc 'value (json-read))))))

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

(my/bind-always "C-c M-g" my/google)
(my/bind-always "C-c t t" my/switch-to-ansi-term)
(my/bind-always "C-c t c" (lambda () (interactive) (ansi-term (getenv "SHELL"))))
(my/bind-always "C-c T l" my/cycle-line-numbers)
(my/bind "C-c Q c" my/chuck-norris-joke)
(my/bind "C-c Q k" my/kanye-west-quote)
(my/bind "C-c Q f" my/fortune)

(provide 'my-misc)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; my-misc.el ends here
