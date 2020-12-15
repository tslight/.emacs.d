;;; change-pairs.el --- change pairs -*- lexical-binding: t; -*-

;;; Commentary:

;; change pairs

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(defun my/change-pairs (from-chars to-chars)
  "Change pairs from FROM-CHARS to TO-CHARS.

When called in Lisp program, FROM-CHARS or TO-CHARS is a string
of bracket pair, eg \"(paren)\", \"[bracket]\", etc.  The first
and last characters are used.

If the string contains “,2”, then the first 2 chars and last 2
chars are used, for example \"[[bracket,2]]\".  If to-chars is
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
           ((string-match ",2" from-chars  )
            (progn
              (setq $fromLeft (substring from-chars 0 2))
              (setq $fromRight (substring from-chars -2))))
           (t
            (progn
              (setq $fromLeft (substring from-chars 0 1))
              (setq $fromRight (substring from-chars -1)))))
          (cond
           ((string-match ",2" to-chars)
            (progn
              (setq $toLeft (substring to-chars 0 2))
              (setq $toRight (substring to-chars -2))))
           ((string-match "none" to-chars)
            (progn
              (setq $toLeft "")
              (setq $toRight "")))
           (t
            (progn
              (setq $toLeft (substring to-chars 0 1))
              (setq $toRight (substring to-chars -1)))))
          (cond
           ((string-match "markdown" from-chars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "`\\([^`]+?\\)`" nil t)
                (replace-match (concat $toLeft "\\1" $toRight ) "FIXEDCASE" ))))
           ((string-match "tilde" from-chars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "~\\([^~]+?\\)~" nil t)
                (replace-match (concat $toLeft "\\1" $toRight ) "FIXEDCASE" ))))
           ((string-match "ascii quote" from-chars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "\"\\([^\"]+?\\)\"" nil t)
                (replace-match (concat $toLeft "\\1" $toRight ) "FIXEDCASE" ))))
           ((string-match "equal" from-chars)
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

(global-set-key (kbd "C-c M-p") 'my/change-pairs)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; change-pairs.el ends here
