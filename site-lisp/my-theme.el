;;; my-theme.el ---

;;; Commentary:

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight tslight@pm.me

;;; Code:
;; -*- lexical-binding: t; -*-
(defun my/after-make-frame (frame)
  (select-frame frame)
  (when (eq system-type 'windows-nt)
    (set-frame-font "Consolas 9" nil t))
  (when (eq system-type 'darwin)
    (set-frame-font "Monaco 10" nil t))
  (when (or (eq system-type 'gnu/linux)
	    (eq system-type 'berkeley-unix))
    (set-frame-font "Monospace 9" nil t))
  (if (display-graphic-p)
      (progn
	(load-theme 'wombat)
	(when (fboundp 'menu-bar-mode)
	  (menu-bar-mode -1))
	(when (fboundp 'scroll-bar-mode)
	  (scroll-bar-mode -1))
	(when (fboundp 'tool-bar-mode)
	  (tool-bar-mode -1))
	(when (fboundp 'tooltip-mode)
	  (tooltip-mode -1)))
    (progn
      (load-theme 'wombat)
      (xterm-mouse-mode 1)
      (mouse-avoidance-mode 'banish)
      (setq linum-format "%d ")
      (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
      (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
      (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
      (global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 1)))
      (global-set-key [mouse-5] '(lambda () (interactive) (scroll-up 1))))))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'my/after-make-frame(selected-frame))
  (my/after-make-frame(selected-frame)))

(defun my/disable-themes ()
  "Disable all custom themes in one fail swoop :-)"
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(setq default-frame-alist
      '((fullscreen . maximized) (vertical-scroll-bars . nil)))

(setq frame-resize-pixelwise t) ;; jwm resize fix

;; mode line stuff
(setq display-time-format "%H:%M %d/%m")
(column-number-mode t)
(display-time-mode 1)
(display-battery-mode 1)
(size-indication-mode t)

(setq prettify-symbols-unprettify-at-point 'right-edge)
(global-prettify-symbols-mode 1)

(define-key my/keymap (kbd "C-c t C-t") 'my/disable-themes)

(provide 'my-theme)
;;; my-theme.el ends here
