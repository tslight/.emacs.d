;;; my-smarter.el --- smarter functions -*- lexical-binding: t; -*-

;;; Commentary:

;; smarter than the defaults

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(defun smart/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'smart/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))
(global-set-key [remap fill-paragraph] #'smart/fill-or-unfill)

(defun smart/narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens, otherwise, it narrows intelligently.

Intelligently means: region, org-src-block, org-subtree, or
defun, whichever applies first.

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
(define-key ctl-x-map "n" #'smart/narrow-or-widen-dwim)

(defun smart/move-beginning-of-line ()
  "Move point back to indentation.

If there is any non blank characters to the left of the cursor.
Otherwise point moves to beginning of line."
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))
(global-set-key [remap move-beginning-of-line] 'smart/move-beginning-of-line)

(defun smart/kill-ring-save ()
  "Copy current line or text selection to kill ring.

When `universal-argument' is called first, copy whole buffer (but
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
(global-set-key [remap kill-ring-save] 'smart/kill-ring-save)

(defun smart/kill-region ()
  "Cut current line, or text selection to kill ring.

When `universal-argument' is called first, cut whole buffer (but
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
(global-set-key [remap kill-region] 'smart/kill-region)

(provide 'my-smarter)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; my-smarter.el ends here
