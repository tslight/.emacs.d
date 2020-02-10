;;; my-edits.el ---

;;; Commentary:

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight tslight@pm.me

;;; Code:
;; -*- lexical-binding: t; -*-
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

(defun my/auto-recompile ()
  "Automatically recompile Emacs Lisp files whenever they are saved."
  (when (equal major-mode 'emacs-lisp-mode)
    (progn
      (byte-compile-file buffer-file-name)
      (message (concat "Re-compiled " buffer-file-name)))))

(defun my/recompile-site-lisp ()
  "Recompile everything in Emacs configuration."
  (interactive)
  (byte-recompile-directory (concat user-emacs-directory "site-lisp") 0 t)
  (byte-compile-file (concat user-emacs-directory "init.el") 0))

(defun my/change-pairs (@from-chars @to-chars)
  "Change bracket pairs between @FROM-CHARS to @TO-CHARS from one
  type to another.

  For example, change all parenthesis () to square brackets [].

  Works on selected text, or current text block.

  When called in Lisp program, @FROM-CHARS or @TO-CHARS is a string
  of bracket pair. eg \"(paren)\", \"[bracket]\", etc.  The first
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
      (ivy-completing-read "From:" $bracketsList )
      (ivy-completing-read "To:" $bracketsList ))))
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

(defun my/copy-isearch-match ()
  (interactive)
  (copy-region-as-kill isearch-other-end (point)))

(defun my/copy-line-or-region ()
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

(defun my/cut-line-or-region ()
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

(defun my/insert-fortune ()
  "Insert output of the fortune command into the buffer, before the point."
  (interactive)
  (insert (shell-command-to-string "fortune")))

(defun my/jump-to-mark ()
  "Jump to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix
argument."
  (interactive)
  (set-mark-command 1))

(defun my/kill-isearch-match ()
  "Kill the current isearch match string and continue searching."
  (interactive)
  (kill-region isearch-other-end (point)))

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

(defun my/smart-beginning-of-line ()
  "Moves point back to indentation if there is any non blank
  characters to the left of the cursor.  Otherwise point moves to
  beginning of line."
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))

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

(defun my/zap-to-isearch (rbeg rend)
  "Kill the region (RBEG - REND) between the mark and the
  closest portion of the isearch match string.

  The behaviour is meant to be analogous to `zap-to-char'; let's
  call it `my/zap-to-isearch'.  The deleted region does not include
  the `isearch' word.  This is meant to be bound only in isearch
  mode.  The point of this function is that oftentimes you want to
  delete some portion of text, one end of which happens to be an
  active isearch word.

  The observation to make is that if you use isearch a lot to move
  the cursor around (as you should, it is much more efficient than
  using the arrows), it happens a lot that you could just delete
  the active region between the mark and the point, not include the
  isearch word."
  (interactive "r")
  (when (not mark-active)
    (error "Mark is not active"))
  (let* ((isearch-bounds (list isearch-other-end (point)))
	 (ismin (apply 'min isearch-bounds))
	 (ismax (apply 'max isearch-bounds)))
    (if (< (mark) ismin)
	(kill-region (mark) ismin)
      (if (> (mark) ismax)
	  (kill-region ismax (mark))
	(error "Internal error in isearch kill function")))
    (isearch-exit)))

(define-key my/keymap (kbd "C-x RET u") 'my/convert-to-unix-coding-system)
(define-key my/keymap (kbd "C-S-SPC") 'my/push-mark-no-activate)
(define-key my/keymap (kbd "C-a") 'my/smart-beginning-of-line)
(define-key my/keymap (kbd "C-w") 'my/copy-line-or-region)
(define-key my/keymap (kbd "M-w") 'my/cut-line-or-region)
(define-key my/keymap (kbd "C-c M-p") 'my/change-pairs)
(define-key my/keymap (kbd "C-c a") 'my/align-symbol)
(define-key my/keymap (kbd "C-c =") 'my/align-equals)
(define-key my/keymap (kbd "C-c :") 'my/align-colon)
(define-key my/keymap (kbd "C-c #") 'my/align-numbers)
(define-key my/keymap (kbd "C-c d") 'my/delete-inside)
(define-key my/keymap (kbd "C-c k") 'my/remove-from-buffer)
(define-key my/keymap (kbd "M-s M-s") 'my/surround)
(define-key my/keymap (kbd "C-o") 'my/open-line-above)
(define-key my/keymap (kbd "C-S-o") 'my/open-line-below)
;; (define-key my/keymap (kbd "C-y") 'my/yank)
;; (define-key my/keymap (kbd "C-M-y") '(lambda () (interactive) (my/yank t)))
(define-key my/keymap (kbd "M-Q") 'my/unfill-region)

(define-key isearch-mode-map (kbd "C-z") 'my/zap-to-isearch)
(define-key isearch-mode-map (kbd "C-w") 'my/copy-isearch-match)
(define-key isearch-mode-map (kbd "M-w") 'my/kill-isearch-match)

(add-hook 'before-save-hook 'my/push-mark-no-activate)
;; (add-hook 'after-save-hook 'my/auto-recompile)
(add-hook 'find-file-hook 'my/hide-dos-eol)

(provide 'my-edits)
;;; my-edits.el ends here
