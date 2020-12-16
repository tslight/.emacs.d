;;; removes.el --- remove crap -*- lexical-binding: t; -*-

;;; Commentary:

;; remove crap from a buffer - eg) ctrl-m dos shit

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
;;;###autoload
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

;;;###autoload
(defun my/remove-character-number (number)
  "Remove all occurences of a control character NUMBER.
Excluding ^I (tabs) and ^J (newline)."
  (if (and (>= number 0) (<= number 31)
           (not (= number 9)) (not (= number 10)))
      (let ((character (string number)))
        (my/remove-from-buffer character))))

;;;###autoload
(defun my/remove-all-ctrl-characters ()
  "Remove all occurences of all control characters.
Excluding ^I (tabs) and ^J (newlines)."
  (interactive)
  (mapcar (lambda (n)
            (my/remove-character-number n))
          (number-sequence 0 31)))

;;;###autoload
(defun my/remove-ctrl-m ()
  "Remove all ^M occurrences from EOL in a buffer."
  (interactive)
  (my/remove-from-buffer "$"))

(global-set-key (kbd "C-c k") 'my/remove-from-buffer)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; removes.el ends here
