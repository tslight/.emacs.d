;;; 00-keys.el ---

;;; Commentary:

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight tslight@pm.me

;;; Code:
;; -*- lexical-binding: t; -*-
(defvar my/keymap (make-sparse-keymap)
  "Keymap for `my/key-mode'.")

;;;###autoload
(define-minor-mode my/key-mode
  "A minor mode so that my key settings override annoying major modes."
  ;; If init-value is not set to t, this mode does not get enabled in
  ;; `fundamental-mode' buffers even after doing \"(my/global-keys-mode 1)\".
  ;; More info: http://emacs.stackexchange.com/q/16693/115
  :global t
  :init-value t
  :lighter " my/key-mode"
  :keymap my/keymap)

;;;###autoload
(define-globalized-minor-mode my/global-keys-mode my/key-mode my/key-mode)

;; https://github.com/jwiegley/use-package/blob/master/bind-key.el
;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists `((my/key-mode . ,my/keymap)))

;; Turn off the minor mode in the minibuffer
(defun turn-off-my/key-mode ()
  "Turn off my/key-mode."
  (my/key-mode -1))
(add-hook 'minibuffer-setup-hook #'turn-off-my/key-mode)

(my/global-keys-mode 1)

(define-key my/keymap (kbd "<f12>") 'recentf-open-files)
(define-key my/keymap (kbd "<f6>") 'desktop-read)
(define-key my/keymap (kbd "M-<escape>") 'other-frame)
(define-key my/keymap (kbd "C-!") 'shell-command)
(define-key my/keymap (kbd "C-$") 'ispell-word)
(define-key my/keymap (kbd "C-%") 'query-replace)
(define-key my/keymap (kbd "C-(") 'insert-parentheses)
(define-key my/keymap (kbd "C-)") 'move-past-close-and-reindent)
(define-key my/keymap (kbd "C-:") 'pp-eval-expression)
(define-key my/keymap (kbd "C-;") 'comment-line)
(define-key my/keymap (kbd "C-<") 'beginning-of-buffer)
(define-key my/keymap (kbd "C-<f10>") 'toggle-frame-maximized)
(define-key my/keymap (kbd "C-<f11>") 'toggle-frame-fullscreen)
(define-key my/keymap (kbd "C-<tab>") 'next-buffer)
(define-key my/keymap (kbd "C-<iso-lefttab>") 'previous-buffer)
(define-key my/keymap (kbd "C->") 'end-of-buffer)
(define-key my/keymap (kbd "C-M-v") 'scroll-other-window-down)
(define-key my/keymap (kbd "C-S-v") 'scroll-other-window)
(define-key my/keymap (kbd "C-^") 'delete-indentation)
(define-key my/keymap (kbd "C-c C-/") 'winner-undo)
(define-key my/keymap (kbd "C-c C-?") 'winner-redo)
(define-key my/keymap (kbd "C-c M-c") 'calculator)
(define-key my/keymap (kbd "C-c c c") 'calc)
(define-key my/keymap (kbd "C-c c n") 'highlight-changes-next-change)
(define-key my/keymap (kbd "C-c c p") 'highlight-changes-previous-change)
(define-key my/keymap (kbd "C-c e b") 'eval-buffer)
(define-key my/keymap (kbd "C-c e c") 'eval-current-buffer)
(define-key my/keymap (kbd "C-c e e") 'pp-eval-last-sexp)
(define-key my/keymap (kbd "C-c e f") 'eval-defun)
(define-key my/keymap (kbd "C-c e m") 'lisp-interaction-mode)
(define-key my/keymap (kbd "C-c e r") 'eval-region)
(define-key my/keymap (kbd "C-c e s") 'eshell)
(define-key my/keymap (kbd "C-c i") 'imenu)
(define-key my/keymap (kbd "C-c t a") 'toggle-text-mode-autofill)
(define-key my/keymap (kbd "C-c t d E") 'toggle-debug-on-entry)
(define-key my/keymap (kbd "C-c t d e") 'toggle-debug-on-error)
(define-key my/keymap (kbd "C-c t d q") 'toggle-debug-on-quit)
(define-key my/keymap (kbd "C-c t l") 'linum-mode)
(define-key my/keymap (kbd "C-c t t") 'toggle-truncate-lines)
(define-key my/keymap (kbd "C-c x") 'execute-extended-command)
(define-key my/keymap (kbd "C-r") 'isearch-backward-regexp)
(define-key my/keymap (kbd "C-s") 'isearch-forward-regexp)
(define-key my/keymap (kbd "C-s-f") 'toggle-frame-fullscreen)
(define-key my/keymap (kbd "C-s-m") 'toggle-frame-maximized)
(define-key my/keymap (kbd "C-w") 'kill-ring-save)
(define-key my/keymap (kbd "C-x C") 'save-buffers-kill-emacs)
(define-key my/keymap (kbd "C-x C-b") 'ibuffer)
(define-key my/keymap (kbd "C-x j") 'jump-to-register)
(define-key my/keymap (kbd "C-x M-k") 'kill-buffer)
(define-key my/keymap (kbd "C-x p") 'pop-to-mark-command)
(define-key my/keymap (kbd "C-z") 'zap-up-to-char)
(define-key my/keymap (kbd "C-~") 'not-modified)
(define-key my/keymap (kbd "M-/") 'hippie-expand)
(define-key my/keymap (kbd "M-;") 'comment-or-uncomment-region)
(define-key my/keymap (kbd "M-<tab>") 'next-multiframe-window)
(define-key my/keymap (kbd "M-<iso-lefttab>") 'previous-multiframe-window)
(define-key my/keymap (kbd "M-SPC") 'cycle-spacing)
(define-key my/keymap (kbd "M-w") 'kill-region)
(define-key my/keymap (kbd "M-z") 'zap-up-char)
(define-key my/keymap (kbd "S-<f10>") 'menu-bar-mode)
(define-key my/keymap (kbd "S-<f6>") 'desktop-save)

;; for help modes, and simple/special modes
(define-key special-mode-map "n" #'forward-button)
(define-key special-mode-map "p" #'backward-button)
(define-key special-mode-map "f" #'forward-button)
(define-key special-mode-map "b" #'backward-button)
(define-key special-mode-map "n" #'widget-forward)
(define-key special-mode-map "p" #'widget-backward)
(define-key special-mode-map "f" #'widget-forward)
(define-key special-mode-map "b" #'widget-backward)

(provide '00-keys)
;;; 00-keys.el ends here
