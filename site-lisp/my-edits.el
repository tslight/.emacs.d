;;; my-edits.el --- my-edits  -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs Configuration

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(defun my/align-symbol (begin end symbol)
  "Align any SYMBOL in region (between BEGIN and END)."
  (interactive "r\nsEnter align symbol: ")
  (align-regexp begin end (concat "\\(\\s-*\\)" symbol) 1 1))

(defun my/align-equals (begin end)
  "Align equals in region (between BEGIN and END)."
  (interactive "r")
  (my/align-symbol begin end "="))

(defun my/align-colon (begin end)
  "Align colons in region (between BEGIN and END)."
  (interactive "r")
  (my/align-symbol begin end ":"))

(defun my/align-numbers (begin end)
  "Align numbers in region (between BEGIN and END)."
  (interactive "r")
  (my/align-symbol begin end "[0-9]+"))

(defadvice align-regexp (around align-regexp-with-spaces activate)
  "Force alignment commands to use spaces, not tabs."
  (let ((indent-tabs-mode nil))
    ad-do-it))

(defun my/auto-recompile ()
  "Automatically recompile Emacs Lisp files whenever they are saved."
  (when (equal major-mode 'emacs-lisp-mode)
    (progn
      (byte-compile-file buffer-file-name t)
      (message (concat "Re-compiled " buffer-file-name)))))

(defun my/recompile-site-lisp ()
  "Recompile everything in Emacs configuration."
  (interactive)
  (byte-recompile-directory (concat user-emacs-directory "site-lisp") 0 t)
  (byte-compile-file (concat user-emacs-directory "init.el") 0))

(defun my/change-pairs (@from-chars @to-chars)
  "Change pairs from @FROM-CHARS to @TO-CHARS.

When called in Lisp program, @FROM-CHARS or @TO-CHARS is a string
of bracket pair, eg \"(paren)\", \"[bracket]\", etc.  The first
and last characters are used.

If the string contains “,2”, then the first 2 chars and last 2
chars are used, for example \"[[bracket,2]]\".  If @to-chars is
equal to string “none”, the brackets are deleted.

If the string has length greater than 2, the rest are ignored."
  (interactive
   (let (($bracketsList
          '("(paren)"
            "{brace}"
            "<greater>"
            "<<double greater,2>>"
            "`emacs'"
            "`markdown`"
            "~tilde~"
            "=equal="
            "\"quote\""
            "[square]"
            "[[double square,2]]"
            "'single quote'"
            "none"
            )))
     (list
      (completing-read "From:" $bracketsList )
      (completing-read "To:" $bracketsList ))))
  (let ( $p1 $p2 )
    (if (use-region-p)
        (progn
          (setq $p1 (region-beginning))
          (setq $p2 (region-end)))
      (save-excursion
        (if (re-search-backward "\n[ \t]*\n" nil "move")
            (progn (re-search-forward "\n[ \t]*\n")
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (if (re-search-forward "\n[ \t]*\n" nil "move")
            (progn (re-search-backward "\n[ \t]*\n")
                   (setq $p2 (point)))
          (setq $p2 (point)))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (let ( (case-fold-search nil)
               $fromLeft
               $fromRight
               $toLeft
               $toRight)
          (cond
           ((string-match ",2" @from-chars  )
            (progn
              (setq $fromLeft (substring @from-chars 0 2))
              (setq $fromRight (substring @from-chars -2))))
           (t
            (progn
              (setq $fromLeft (substring @from-chars 0 1))
              (setq $fromRight (substring @from-chars -1)))))
          (cond
           ((string-match ",2" @to-chars)
            (progn
              (setq $toLeft (substring @to-chars 0 2))
              (setq $toRight (substring @to-chars -2))))
           ((string-match "none" @to-chars)
            (progn
              (setq $toLeft "")
              (setq $toRight "")))
           (t
            (progn
              (setq $toLeft (substring @to-chars 0 1))
              (setq $toRight (substring @to-chars -1)))))
          (cond
           ((string-match "markdown" @from-chars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "`\\([^`]+?\\)`" nil t)
                (replace-match (concat $toLeft "\\1" $toRight ) "FIXEDCASE" ))))
           ((string-match "tilde" @from-chars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "~\\([^~]+?\\)~" nil t)
                (replace-match (concat $toLeft "\\1" $toRight ) "FIXEDCASE" ))))
           ((string-match "ascii quote" @from-chars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "\"\\([^\"]+?\\)\"" nil t)
                (replace-match (concat $toLeft "\\1" $toRight ) "FIXEDCASE" ))))
           ((string-match "equal" @from-chars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "=\\([^=]+?\\)=" nil t)
                (replace-match (concat $toLeft "\\1" $toRight ) "FIXEDCASE" ))))
           (t (progn
                (progn
                  (goto-char (point-min))
                  (while (search-forward $fromLeft nil t)
                    (replace-match $toLeft "FIXEDCASE" "LITERAL")))
                (progn
                  (goto-char (point-min))
                  (while (search-forward $fromRight nil t)
                    (replace-match $toRight "FIXEDCASE" "LITERAL")))))))))))

(defun my/convert-to-unix-coding-system ()
  "Change the current buffer's file encoding to unix."
  (interactive)
  (let ((coding-str (symbol-name buffer-file-coding-system)))
    (when (string-match "-\\(?:dos\\|mac\\)$" coding-str)
      (set-buffer-file-coding-system 'unix))))

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

(defun my/hide-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun my/insert-date ()
  "Insert a timestamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(defun my/isearch-exit ()
  "Move point to the start of the matched string, regardless of
search direction. A.K.A. Vim style."
  (interactive)
  (when (eq isearch-forward t)
    (goto-char isearch-other-end))
  (isearch-exit))

(defun my/copy-to-isearch ()
  "Copy up to the search match when searching forward. When
searching backward, copy to the start of the search match."
  (interactive)
  (my/isearch-exit)
  (call-interactively 'kill-ring-save)
  (exchange-point-and-mark))

(defun my/kill-to-isearch ()
  "Kill up to the search match when searching forward. When
searching backward, kill to the beginning of the match."
  (interactive)
  (my/isearch-exit)
  (call-interactively 'kill-region))

(define-key isearch-mode-map (kbd "<return>") 'my/isearch-exit)
(define-key isearch-mode-map (kbd "C-w") 'my/copy-to-isearch)
(define-key isearch-mode-map (kbd "M-w") 'my/kill-to-isearch)

(defun my/jump-to-mark ()
  "Jump to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix
argument."
  (interactive)
  (set-mark-command 1))

(defun my/narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows
  intelligently.  Intelligently means: region, org-src-block,
  org-subtree, or defun, whichever applies first.

  Narrowing to org-src-block actually calls `org-edit-src-code'.
  With prefix P, don't widen, just narrow even if buffer is already
  narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing command.
         ;; Remove this first conditional if you don't want it.
         (cond ((ignore-errors (org-edit-src-code))
                (delete-other-windows))
               ((org-at-block-p)
                (org-narrow-to-block))
               (t (org-narrow-to-subtree))))
        (t (narrow-to-defun))))
;; (define-key endless/toggle-map "n" #'narrow-or-widen-dwim)
;; This line actually replaces Emacs' entire narrowing keymap, that's
;; how much I like this command. Only copy it if that's what you want.
(define-key ctl-x-map "n" #'my/narrow-or-widen-dwim)

(defun my/open-line-above ()
  "Insert an empty line above the current line.  Position the
  cursor at its beginning, according to the current mode."
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
  "Insert an empty line after the current line. Position the
  cursor at its beginning, according to the current mode.  With a
  prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (my/open-line-above)
    (move-end-of-line nil)
    (newline-and-indent)))

(defun my/push-mark-no-activate ()
  "Push `point' to `mark-ring', but do not activate the region.
  Equivalent to \\[set-mark-command] when \\[transient-mark-mode]
  is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

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

(defun smart/move-beginning-of-line ()
  "Moves point back to indentation if there is any non blank
characters to the left of the cursor.  Otherwise point moves to
beginning of line."
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))

(defun smart/kill-ring-save ()
  "Copy current line or text selection to kill ring.  When
`universal-argument' is called first, copy whole buffer (but
respect `narrow-to-region')."
  (interactive)
  (let (p1 p2)
    (if (null current-prefix-arg)
        (progn (if (use-region-p)
                   (progn (setq p1 (region-beginning))
                          (setq p2 (region-end)))
                 (progn (setq p1 (line-beginning-position))
                        (setq p2 (line-end-position)))))
      (progn (setq p1 (point-min))
             (setq p2 (point-max))))
    (kill-ring-save p1 p2)))

(defun smart/kill-region ()
  "Cut current line, or text selection to kill ring.  When
`universal-argument' is called first, cut whole buffer (but
respect `narrow-to-region')."
  (interactive)
  (let (p1 p2)
    (if (null current-prefix-arg)
        (progn (if (use-region-p)
                   (progn (setq p1 (region-beginning))
                          (setq p2 (region-end)))
                 (progn (setq p1 (line-beginning-position))
                        (setq p2 (line-beginning-position 2)))))
      (progn (setq p1 (point-min))
             (setq p2 (point-max))))
    (kill-region p1 p2)))

(global-set-key [remap move-beginning-of-line] 'smart/move-beginning-of-line)
(global-set-key [remap kill-ring-save] 'smart/kill-ring-save)
(global-set-key [remap kill-region] 'smart/kill-region)

(defun my/sort-lines-nocase ()
  "Sort marked lines with case sensitivity."
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))

(defun my/surround (begin end open close)
  "Put OPEN at BEGIN and CLOSE at END of the region.  If you omit
  CLOSE, it will reuse OPEN."
  (interactive  "r\nsStart: \nsEnd: ")
  (save-excursion
    (goto-char end)
    (if (string= close "")
        (insert open)
      (insert close))
    (goto-char begin)
    (insert open)))

(defun my/underline-text (arg)
  "Inserts a line under the current line, filled with a default
underline character `='. If point had been at the end of the
line, moves point to the beginning of the line directly following
the underlining. It does not underline the line's leading
whitespace, trailing whitespace, or comment symbols. With prefix
`C-u' prompts user for a custom underline character. With prefix
`C-u C-u', does not underline whitespace embedded in the line."
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

(defun my/unfill-region (&optional region)
  "Take a multi-line paragraph, or REGION, and make it into a
  single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(defun my/untabify-buffer ()
  "Convert all tabs to spaces in the buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun my/yank (&optional forwards)
  "This command calls `yank', and if repeated, calls `yank-pop'.

  When `universal-argument' is called first with a number arg,
  paste that many times.

  If called with `optional' `forwards' set to true, call `yank-pop'
  with -1."
  (interactive)
  (progn
    (when (and delete-selection-mode (region-active-p))
      (delete-region (region-beginning) (region-end)))
    (if current-prefix-arg
        (progn
          (dotimes ($i (prefix-numeric-value current-prefix-arg))
            (yank)))
      (if (eq real-last-command this-command)
          (if forwards
              (yank-pop -1)
            (yank-pop 1))
        (yank)))))

(defun my/xml-pretty-print ()
  "Reformat and indent XML."
  (interactive)
  (save-excursion
    (sgml-pretty-print (point-min) (point-max))
    (indent-region (point-min) (point-max))))

(defun my/change-number-at-point (change)
  (let ((number (number-at-point))
        (point (point)))
    (when number
      (progn
        (forward-word)
        (search-backward (number-to-string number))
        (replace-match (number-to-string (funcall change number)))
        (goto-char point)))))

(defun my/increment-number-at-point ()
  "Increment number at point like vim's C-a"
  (interactive)
  (my/change-number-at-point '1+))

(defun my/decrement-number-at-point ()
  "Decrement number at point like vim's C-x"
  (interactive)
  (my/change-number-at-point '1-))

(my/bind-always "C-x RET u" my/convert-to-unix-coding-system)
(my/bind-always "C-S-SPC" my/push-mark-no-activate)
(my/bind-always "C-c M-p" my/change-pairs)
(my/bind-always "C-c a" my/align-symbol)
(my/bind-always "C-c =" my/align-equals)
(my/bind-always "C-c :" my/align-colon)
(my/bind-always "C-c #" my/align-numbers)
(my/bind-always "C-c d" my/delete-inside)
(my/bind-always "C-c k" my/remove-from-buffer)
(my/bind-always "C-c u" my/underline-text)
(my/bind-always "M-s M-s" my/surround)
(my/bind-always "C-o" my/open-line-above)
(my/bind-always "C-S-o" my/open-line-below)
;; (my/bind-always "C-y" my/yank)
;; (my/bind-always "C-M-y" (lambda () (interactive) (my/yank t)))
(my/bind-always "M-Q" my/unfill-region)
(my/bind "C-c +" my/increment-number-at-point)
(my/bind "C-c -" my/decrement-number-at-point)

(add-hook 'before-save-hook 'my/push-mark-no-activate)
(add-hook 'after-save-hook 'my/auto-recompile)
(add-hook 'find-file-hook 'my/hide-dos-eol)

(provide 'my-edits)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; my-edits.el ends here
