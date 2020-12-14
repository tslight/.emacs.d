;;; early-init-whitespace.el --- whitespace settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Whitespace configuration and hooks

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <toby@probook>

;;; Code:
(setq whitespace-line-column 120)
(setq whitespace-style '(face
                         tabs
                         spaces
                         trailing
                         lines
                         space-before-tab::space
                         newline
                         indentation::space
                         empty
                         space-after-tab::space
                         space-mark
                         tab-mark
                         newline-mark)
      whitespace-face 'whitespace-trailing)

(global-set-key (kbd "C-c M-w") 'whitespace-mode)

(add-hook 'before-save-hook 'whitespace-cleanup)
(provide 'early-init-whitespace)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; early-init-whitespace.el ends here
