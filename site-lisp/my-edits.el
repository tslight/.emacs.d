;;; my-edits.el --- my-edits  -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs Configuration

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(defun my/delete-inside ()
  "Deletes the text within parentheses, brackets or quotes."
  (interactive)
  ;; Search for a match on the same line, don't delete across lines
  (search-backward-regexp "[[{(<\"\']" (line-beginning-position))
  (forward-char)
  (let ((lstart (point)))
    (search-forward-regexp "[]})>\"\']" (line-end-position))
    (backward-char)
    (kill-region lstart (point))))

(defun my/generate-numbered-list (start end char)
  "Create a numbered list from START to END.  Using CHAR as punctuation."
  (interactive "nStart number:\nnEnd number:\nsCharacter:")
  (let ((x start))
    (while (<= x end)
      (insert (concat (number-to-string x) char))
      (newline)
      (setq x (+ x 1)))))

(defun my/insert-date ()
  "Insert a timestamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(defun my/open-line-above ()
  "Insert an empty line above the current line.

Position the cursor at its beginning, according to the current
mode."
  (interactive)
  (move-beginning-of-line nil)
  (insert "\n")
  (if electric-indent-inhibit
      ;; We can't use `indent-according-to-mode' in languages like Python,
      ;; as there are multiple possible indentations with different meanings.
      (let* ((indent-end (progn (move-to-mode-line-start) (point)))
             (indent-start (progn (move-beginning-of-line nil) (point)))
             (indent-chars (buffer-substring indent-start indent-end)))
        (forward-line -1)
        ;; This new line should be indented with the same characters as
        ;; the current line.
        (insert indent-chars))
    ;; Just use the current major-mode's indent facility.
    (forward-line -1)
    (indent-according-to-mode)))

(defun my/open-line-below (arg)
  "Insert an empty line after the current line.

Position the cursor at its beginning, according to the current
mode.  With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (my/open-line-above)
    (move-end-of-line nil)
    (newline-and-indent)))

(defun my/sort-lines-nocase ()
  "Sort marked lines with case sensitivity."
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))

(defun my/surround (begin end open close)
  "Put OPEN at BEGIN and CLOSE at END of the region.

If you omit CLOSE, it will reuse OPEN."
  (interactive  "r\nsStart: \nsEnd: ")
  (save-excursion
    (goto-char end)
    (if (string= close "")
        (insert open)
      (insert close))
    (goto-char begin)
    (insert open)))

(defun my/underline-text (arg)
  "Insert ARG under the current line.

Filled with a default underline character `='.

If point had been at the end of the line, moves point to the
beginning of the line directly following the underlining.

It does not underline the line's leading whitespace, trailing
whitespace, or comment symbols.

With prefix prompts user for a custom underline character.

With double prefix, does not underline whitespace embedded in the
line."
  (interactive "p")
  (let* ((original-point (point))
         (underline-char
          (replace-regexp-in-string "[[:cntrl:][:space:]]" "="
                                    (if (= arg 1)
                                        "="
                                      (char-to-string
                                       (read-char "What character to underline with?")))))
         (original-point-is-eol
          (when (looking-at "$") t))
         (original-point-is-eob
          (= original-point (point-max))))
    (beginning-of-line)
    (unless
        (when (looking-at "[[:space:]]*$")
          (beginning-of-line 0)
          (when (looking-at "[[:space:]]*$")
            (goto-char original-point)
            (message "nothing to do")))
      (insert
       (buffer-substring (line-beginning-position) (line-end-position))
       "\n")
      (save-restriction
        (narrow-to-region
         (progn
           (goto-char (1- (re-search-forward "[^[:space:]]" nil t)))
           (cond
            ((looking-at ";+")   (match-end 0))
            ((looking-at "#+")   (match-end 0))
            ((looking-at "//+")  (match-end 0))
            ((looking-at "/\\*+") (match-end 0))
            (t (point))))
         (1+ (progn
               (goto-char (line-end-position))
               (re-search-backward "[^[:space:]]" nil t))))
        (untabify (point-min) (point-max))
        (goto-char (point-min))
        (if (= arg 16)
            (while (re-search-forward "[^[:space:]]" nil t)
              (replace-match underline-char nil))
          (re-search-forward "[^[:space:]]" nil t)
          (goto-char (1- (point)))
          (while (re-search-forward "." nil t)
            (replace-match underline-char nil)))
        (widen))
      (if original-point-is-eob
          (goto-char (point-max))
        (if original-point-is-eol
            (goto-char (re-search-forward "^"))
          (goto-char original-point))))))

(defun my/untabify-buffer ()
  "Convert all tabs to spaces in the buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun my/xml-pretty-print ()
  "Reformat and indent XML."
  (interactive)
  (save-excursion
    (sgml-pretty-print (point-min) (point-max))
    (indent-region (point-min) (point-max))))

(defun my/yank-pop-forwards (arg)
  "Cycle forwards through the kill.  Reverse `yank-pop'.  With ARG."
  (interactive "p")
  (yank-pop (- arg)))

(provide 'my-edits)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; my-edits.el ends here
