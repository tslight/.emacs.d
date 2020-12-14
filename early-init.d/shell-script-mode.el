;;; early-init-shell-script-mode.el --- shell script mode settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Shell Script Mode Settings

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(add-hook 'shell-script-mode-hook 'display-line-numbers-mode)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-to-list 'auto-mode-alist '("\\.sh\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.bash.*\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.zsh.*\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\bashrc\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\kshrc\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\profile\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\zshenv\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\zprompt\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\zshrc\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\prompt_.*_setup\\'" . shell-script-mode))
(add-to-list 'interpreter-mode-alist '("bash" . shell-script-mode))
(add-to-list 'interpreter-mode-alist '("ksh" . shell-script-mode))
(add-to-list 'interpreter-mode-alist '("sh" . shell-script-mode))
(add-to-list 'interpreter-mode-alist '("zsh" . shell-script-mode))
(provide 'early-init-shell-script-mode)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; early-init-shell-script-mode.el ends here
