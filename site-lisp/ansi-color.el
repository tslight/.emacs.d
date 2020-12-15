;;; ansi-color.el --- ansi color tweaks -*- lexical-binding: t; -*-

;;; Commentary:

;; ANSI color tweaks

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(defun colorize-compilation-buffer ()
  "ANSI color in compilation buffer."
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; ansi-color.el ends here
