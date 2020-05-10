;;; keys.el --- keys  -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs Configuration

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(defvar my/keymap (make-sparse-keymap)
  "Keymap for `my/key-mode'.")

;;;###autoload
(define-minor-mode my/key-mode
  "A minor mode so that my key settings override annoying major modes."
  :global t
  :init-value t
  :lighter " my/key-mode"
  :keymap my/keymap)

;;;###autoload
(define-globalized-minor-mode my/global-keys-mode my/key-mode my/key-mode)

(add-to-list 'emulation-mode-map-alists `((my/key-mode . ,my/keymap)))

(defun turn-off-my/key-mode ()
  "Turn off my/key-mode in the minibuffer."
  (my/key-mode -1))

(add-hook 'minibuffer-setup-hook #'turn-off-my/key-mode)

(my/global-keys-mode 1)

(defmacro my/bind (key function)
  "Bind a KEY to a FUNCTION at the global level."
  `(global-set-key (kbd ,key) ',function))

(defmacro my/bind-always (key function)
  "Bind a KEY to a FUNCTION at the global level - always."
  `(define-key my/keymap (kbd ,key) ',function))

(my/bind-always "C-!" shell-command)
(my/bind-always "C-$" ispell-word)
(my/bind-always "C-%" query-replace)
(my/bind-always "C-(" insert-parentheses)
(my/bind-always "C-)" move-past-close-and-reindent)
(my/bind-always "C-:" pp-eval-expression)
(my/bind-always "C-;" comment-line)
(my/bind-always "M-;" comment-or-uncomment-region)
(my/bind-always "C-<" beginning-of-buffer)
(my/bind-always "C->" end-of-buffer)
(my/bind-always "C-^" delete-indentation)
(my/bind-always "C-~" not-modified)
(my/bind-always "C-w" kill-ring-save)
(my/bind-always "M-w" kill-region)
(my/bind-always "C-z" zap-up-to-char)
(my/bind-always "M-z" zap-up-char)

(my/bind-always "C-c e b" (lambda () (interactive)
                            (eval-buffer)
                            (message "Evaluated %s" (current-buffer))))
(my/bind-always "C-c e e" pp-eval-last-sexp)
(my/bind-always "C-c e f" (lambda () (interactive)
                            (eval-buffer)
                            (message "Evaluated function.")))
(my/bind-always "C-c e m" lisp-interaction-mode)
(my/bind-always "C-c e r" (lambda () (interactive)
                            (eval-buffer)
                            (message "Evaluated region.")))
(my/bind-always "C-c e s" eshell)

(my/bind-always "C-c M-t a" toggle-text-mode-autofill)
(my/bind-always "C-c M-t d E" toggle-debug-on-entry)
(my/bind-always "C-c M-t d e" toggle-debug-on-error)
(my/bind-always "C-c M-t d q" toggle-debug-on-quit)
(my/bind-always "C-c M-t t" toggle-truncate-lines)

(my/bind-always "C-S-n" highlight-changes-next-change)
(my/bind-always "C-S-p" highlight-changes-previous-change)

(my/bind-always "C-x c" save-buffers-kill-emacs)
(my/bind-always "C-x M-c" save-buffers-kill-emacs)
(my/bind-always "C-x C-b" ibuffer)
(my/bind-always "C-x M-k" kill-buffer)

(when (>= emacs-major-version 27)
  (my/bind-always "C-x t t" tab-bar-select-tab-by-name)
  (my/bind-always "C-S-t" tab-bar-new-tab)
  (my/bind-always "C-x t c" tab-bar-new-tab)
  (my/bind-always "C-S-w" tab-bar-close-tab)
  (my/bind-always "C-x t k" tab-bar-close-tab)
  (my/bind-always "C-<tab>" tab-bar-switch-to-next-tab)
  (my/bind-always "C-x t n" tab-bar-switch-to-next-tab)
  (my/bind-always "C-<iso-lefttab>" tab-bar-switch-to-prev-tab)
  (my/bind-always "C-x t p" tab-bar-switch-to-prev-tab)
  (my/bind-always "C-x t l" tab-bar-switch-to-recent-tab)
  (my/bind-always "C-`" tab-bar-switch-to-recent-tab))

(my/bind-always "C-<f10>" toggle-frame-maximized)
(my/bind-always "C-<f11>" toggle-frame-fullscreen)
(my/bind-always "C-s-f" toggle-frame-fullscreen)
(my/bind-always "C-s-m" toggle-frame-maximized)
(my/bind-always "M-<escape>" other-frame)
(my/bind-always "M-<tab>" next-multiframe-window)
(my/bind-always "M-<iso-lefttab>" previous-multiframe-window)

(my/bind "C-r" isearch-backward-regexp)
(my/bind "C-s" isearch-forward-regexp)
(my/bind "C-M-r" multi-isearch-buffers-regexp)
(my/bind "C-S-r" multi-isearch-files-regexp)
(my/bind "C-M-s" isearch-forward-symbol-at-point)

(my/bind-always "<f12>" recentf-open-files)
(my/bind-always "<f6>" desktop-read)
(my/bind-always "S-<f6>" desktop-save)
(my/bind-always "S-<f10>" menu-bar-mode)
(my/bind-always "C-S-v" scroll-other-window-down)
;; (my/bind-always "C-M-v" scroll-other-window)
(my/bind-always "C-c C-/" winner-undo)
(my/bind-always "C-c C-?" winner-redo)
(my/bind-always "C-c M-c" calculator)
(my/bind-always "C-c c" calc)
(my/bind-always "C-c i" imenu)
(my/bind-always "C-c x" execute-extended-command)
(my/bind-always "C-x j" jump-to-register)
(my/bind-always "C-x p" pop-to-mark-command)
(my/bind-always "M-SPC" cycle-spacing)
(my/bind-always "M-/" hippie-expand)

;; for help modes, and simple/special modes
(define-key special-mode-map "n" #'forward-button)
(define-key special-mode-map "p" #'backward-button)
(define-key special-mode-map "f" #'forward-button)
(define-key special-mode-map "b" #'backward-button)
(define-key special-mode-map "n" #'widget-forward)
(define-key special-mode-map "p" #'widget-backward)
(define-key special-mode-map "f" #'widget-forward)
(define-key special-mode-map "b" #'widget-backward)

(provide 'keys)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; keys.el ends here
