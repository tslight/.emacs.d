;;; misc.el --- misc  -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs Configuration

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
;;;###autoload
(defun my/substring (substring string)
  "Return SUBSTRING of a STRING."
  (let ((regex (concat  ".*\\(" substring "\\).*")))
    (string-match regex string)
    (match-string 1 string)))

;;;###autoload
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

;;;###autoload
(defun my/fortune ()
  "Insert a fortune into the minibuffer.

If called with `prefix-arg', insert output of the fortune command
into the buffer, before the point."
  (interactive)
  (if current-prefix-arg
      (insert (shell-command-to-string "fortune"))
    (message (string-trim (shell-command-to-string "fortune -s -n 100")))))

;;;###autoload
(defun my/google (arg)
  "Googles a query or region.  With prefix ARG, wrap in quotes."
  (interactive "P")
  (let ((query
         (if (region-active-p)
             (buffer-substring (region-beginning) (region-end))
           (read-string "Query: "))))
    (when arg (setq query (concat "\"" query "\"")))
    (browse-url
     (concat "http://www.google.com/search?ie=utf-8&oe=utf-8&q=" query))))

;;;###autoload
(defun my/kanye-west-quote ()
  "Get a random Kanye quote in the minibuffer."
  (interactive)
  (message
   (with-temp-buffer
     (url-insert-file-contents "https://api.kanye.rest/")
     (cdr (assoc 'quote (json-read))))))

;;;###autoload
(defun my/chuck-norris-joke ()
  "Get a random Chuck Norris joke in the minibuffer."
  (interactive)
  (message
   (with-temp-buffer
     (url-insert-file-contents "https://api.chucknorris.io/jokes/random")
     (cdr (assoc 'value (json-read))))))

;;;###autoload
(defmacro my/measure-time (&rest body)
  "Measure and return the running time of BODY."
  (declare (indent defun))
  (let ((start (make-symbol "start")))
    `(let ((,start (float-time)))
       ,@body
       (- (float-time) ,start))))

(global-set-key (kbd "C-c M-g") 'my/google)
(global-set-key (kbd "C-c M-t l") 'my/cycle-line-numbers)
(global-set-key (kbd "C-c Q c") 'my/chuck-norris-joke)
(global-set-key (kbd "C-c Q k") 'my/kanye-west-quote)
(global-set-key (kbd "C-c Q f") 'my/fortune)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; misc.el ends here
