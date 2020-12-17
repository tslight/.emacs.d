(setq icomplete-prospects-height 6)
(setq icomplete-separator "\n")

(defun icomplete-vertical-minibuffer-setup ()
  (setq truncate-lines t)
  (setq-local completion-ignore-case t)
  (setq-local read-file-name-completion-ignore-case t)
  (setq-local read-buffer-completion-ignore-case t)
  (setq icomplete-hide-common-prefix nil))

(add-hook 'icomplete-minibuffer-setup-hook #'icomplete-vertical-minibuffer-setup)

(defun icomplete-vertical-reformat-completions (completions)
  (save-match-data
    (let ((cnp (substring-no-properties completions)))
      (if (string-match "^\\((.*)\\|\\[.+\\]\\)?{\\(\\(?:.\\|\n\\)+\\)}" cnp)
          (format "%s \n%s"
                  (or (match-string 1 cnp) "")
                  (replace-regexp-in-string "^" (make-string (current-column) ?
                                                             ) (match-string 2 cnp)))
        cnp))))

(defun icomplete-vertical-adjust-minibuffer-height (completions)
  (let* ((comp (icomplete-vertical-reformat-completions completions))
         (complen (length (split-string comp "\n"))))
    (if (> complen 1) (enlarge-window (- icomplete-prospects-height (1-
                                                                     (window-height)))))
    comp))

(advice-add 'icomplete-completions :filter-return
            #'icomplete-vertical-adjust-minibuffer-height)
