;;; my-misc.el --- my-misc  -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs Configuration

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(defun my/substring (substring string)
  "Return SUBSTRING of a STRING."
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

(provide 'my-misc)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; my-misc.el ends here
