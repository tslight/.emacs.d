;;; my-removes.el --- remove crap -*- lexical-binding: t; -*-

;;; Commentary:

;; remove crap from a buffer - eg) ctrl-m dos shit

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(defun my/remove-from-buffer (string)
  "Remove all occurences of STRING from the whole buffer."
  (interactive "sString to remove: ")
  (save-match-data
    (save-excursion
      (let ((count 0))
        (goto-char (point-min))
        (while (re-search-forward string (point-max) t)
          (setq count (+ count 1))
          (replace-match "" nil nil))
        (message (format "%d %s removed from buffer." count string))))))

(defun my/remove-character-number (number)
  "Remove all occurences of a control character NUMBER from a
  buffer (excluding ^I (tabs) and ^J (newline)."
  (if (and (>= number 0) (<= number 31)
           (not (= number 9)) (not (= number 10)))
      (let ((character (string number)))
        (my/remove-from-buffer character))))

(defun my/remove-all-ctrl-characters ()
  "Remove all occurences of all control characters from a
  buffer (excluding ^I (tabs) and ^J (newlines)."
  (interactive)
  (mapcar (lambda (n)
            (my/remove-character-number n))
          (number-sequence 0 31)))

(defun my/remove-ctrl-m ()
  "Remove all ^M occurrences from EOL in a buffer."
  (interactive)
  (my/remove-from-buffer "$"))

(my/bind-always "C-c k" my/remove-from-buffer)

(provide 'my-removes)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; my-removes.el ends here
