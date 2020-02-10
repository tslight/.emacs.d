;;; my-windows.el ---

;;; Commentary:

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight tslight@pm.me

;;; Code:
;; -*- lexical-binding: t; -*-
(defun my/three-way-split ()
  "Split the screen three ways."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer)
  (balance-windows))

(defun my/kill-buffer-other-window ()
  "Kill the buffer in the last used window."
  (interactive)
  ;; Window selection is used because point goes to a different window if more
  ;; than 2 windows are present
  (let ((current-window (selected-window))
	(other-window (get-mru-window t t t)))
    (select-window other-window)
    (kill-this-buffer)
    (select-window current-window)))

(defun my/last-window ()
  "Switch back and forth between two windows easily."
  (interactive)
  (let ((win (get-mru-window t t t)))
    (unless win (error "Last window not found"))
    (let ((frame (window-frame win)))
      (raise-frame frame)
      (select-frame frame)
      (select-window win))))

(defun my/open-buffer-other-window (buffer)
  "Open a buffer in another window without switching to it."
  (interactive "BBuffer: ")
  (switch-to-buffer-other-window buffer)
  (other-window -1))

(defun my/prev-window ()
  "Go the previously used window, excluding other frames."
  (interactive)
  (other-window -1))

(defun my/top-of-window ()
  "Shift current line to the top of the window-  i.e. zt in Vim"
  (interactive)
  (set-window-start (selected-window) (point)))

(defun my/bottom-of-window ()
  "Shift current line to the bottom of the window- i.e. zb in Vim"
  (interactive)
  (my/top-of-window)
  (scroll-down (- (window-height) 3)))

(defun my/scroll-line-up (n)
  "Equivalent of Ctrl-e in Vi."
  (interactive "p")
  (scroll-up n))

(defun my/scroll-line-down (n)
  "Equivalent of Ctrl-y in Vi."
  (interactive "p")
  (scroll-down n))

(defun my/hsplit-last-buffer (prefix)
  "Split the window vertically and display the previous buffer.
With PREFIX stay in current buffer."
  (interactive "p")
  (split-window-vertically)
  (other-window 1 nil)
  (if (= prefix 1)
      (switch-to-next-buffer)))

(defun my/vsplit-last-buffer (prefix)
  "Split the window horizontally and display the previous buffer.
With PREFIX stay in current buffer."
  (interactive "p")
  (split-window-horizontally)
  (other-window 1 nil)
  (if (= prefix 1) (switch-to-next-buffer)))

(defun my/toggle-split ()
  "Switch window split from horizontally to vertically, or vice
versa.  Change right window to bottom, or change bottom window to
right."
  (interactive)
  (require 'windmove)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
	(let* ((win (selected-window))
	       (nextdir (car dirs))
	       (neighbour-dir (cdr dirs))
	       (next-win (windmove-find-other-window nextdir win))
	       (neighbour1 (windmove-find-other-window neighbour-dir win))
	       (neighbour2 (if next-win (with-selected-window next-win
					  (windmove-find-other-window neighbour-dir next-win)))))
	  ;;(message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
	  (setq done (and (eq neighbour1 neighbour2)
			  (not (eq (minibuffer-window) next-win))))
	  (if done
	      (let* ((other-buf (window-buffer next-win)))
		(delete-window next-win)
		(if (eq nextdir 'right)
		    (split-window-vertically)
		  (split-window-horizontally))
		(set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))

(define-key my/keymap (kbd "C-x O") 'my/prev-window)
(define-key my/keymap (kbd "C-c 3") 'my/vsplit-last-buffer)
(define-key my/keymap (kbd "C-c 2") 'my/hsplit-last-buffer)
(define-key my/keymap (kbd "M-<") 'my/top-of-window)
(define-key my/keymap (kbd "M->") 'my/bottom-of-window)
(define-key my/keymap (kbd "M-p") 'my/scroll-line-up)
(define-key my/keymap (kbd "M-n") 'my/scroll-line-down)

(define-key ctl-x-4-map "k" 'my/kill-buffer-other-window)
(define-key ctl-x-4-map "o" 'my/open-buffer-other-window)
(define-key ctl-x-4-map "t" 'my/toggle-split)

(provide 'my-windows)
;;; my-windows.el ends here
