;;; bind.el --- binding macros -*- lexical-binding: t; -*-

;;; Commentary:

;; binding macros

;; Copyright (C) 2020 Toby Slight
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

(provide 'bind)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; bind.el ends here
