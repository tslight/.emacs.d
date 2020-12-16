;;; windows.el --- windows  -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs Configuration

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(require 'windmove)

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun my/last-window ()
  "Switch back and forth between two windows easily."
  (interactive)
  (let ((win (get-mru-window t t t)))
    (unless win (error "Last window not found"))
    (let ((frame (window-frame win)))
      (raise-frame frame)
      (select-frame frame)
      (select-window win))))

;;;###autoload
(defun my/open-buffer-other-window (buffer)
  "Open a BUFFER in another window without switching to it."
  (interactive "BBuffer: ")
  (switch-to-buffer-other-window buffer)
  (other-window -1))

;;;###autoload
(defun my/prev-window ()
  "Go the previously used window, excluding other frames."
  (interactive)
  (other-window -1))

;;;###autoload
(defun my/top-of-window ()
  "Shift current line to the top of the window."
  (interactive)
  (set-window-start (selected-window) (point)))

;;;###autoload
(defun my/bottom-of-window ()
  "Shift current line to the bottom of the window."
  (interactive)
  (my/top-of-window)
  (scroll-down (- (window-height) 3)))

;;;###autoload
(defun my/scroll-line-up (n)
  "Scroll line up N lines.  Like Ctrl-e in Vim."
  (interactive "p")
  (scroll-up n))

;;;###autoload
(defun my/scroll-line-down (n)
  "Scroll line down N lines.  Ctrl-y in Vim."
  (interactive "p")
  (scroll-down n))

;;;###autoload
(defun my/hsplit-last-buffer (prefix)
  "Split the window vertically and display the previous buffer.
With PREFIX stay in current buffer."
  (interactive "p")
  (split-window-vertically)
  (other-window 1 nil)
  (if (= prefix 1)
      (switch-to-next-buffer)))

;;;###autoload
(defun my/vsplit-last-buffer (prefix)
  "Split the window horizontally and display the previous buffer.
With PREFIX stay in current buffer."
  (interactive "p")
  (split-window-horizontally)
  (other-window 1 nil)
  (if (= prefix 1) (switch-to-next-buffer)))

;;;###autoload
(defun my/toggle-split ()
  "Switch window split from horizontally to vertically.

Or vice versa.  Change right window to bottom, or change bottom
window to right."
  (interactive)
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

;;;###autoload
(defun my/transpose-windows (arg)
  "Transpose windows.  Use prefix ARG to transpose in the other direction."
  (interactive "P")
  (if (not (> (count-windows) 1))
      (message "You can't rotate a single window!")
    (let* ((rotate-times (prefix-numeric-value arg))
           (direction (if (or (< rotate-times 0) (equal arg '(4)))
                          'reverse 'identity)))
      (dotimes (_ (abs rotate-times))
        (dotimes (i (- (count-windows) 1))
          (let* ((w1 (elt (funcall direction (window-list)) i))
                 (w2 (elt (funcall direction (window-list)) (+ i 1)))
                 (b1 (window-buffer w1))
                 (b2 (window-buffer w2))
                 (s1 (window-start w1))
                 (s2 (window-start w2))
                 (p1 (window-point w1))
                 (p2 (window-point w2)))
            (set-window-buffer-start-and-point w1 b2 s2 p2)
            (set-window-buffer-start-and-point w2 b1 s1 p1)))))))

(global-set-key (kbd "C-x O") 'my/prev-window)
(global-set-key (kbd "C-c 3") 'my/vsplit-last-buffer)
(global-set-key (kbd "C-c 2") 'my/hsplit-last-buffer)
(global-set-key (kbd "C-M-<") 'my/top-of-window)
(global-set-key (kbd "C-M->") 'my/bottom-of-window)
(global-set-key (kbd "M-p") 'my/scroll-line-up)
(global-set-key (kbd "M-n") 'my/scroll-line-down)

(global-set-key (kbd "C-c v") 'scroll-other-window-down)
(global-set-key (kbd "C-c C-/") 'winner-undo)
(global-set-key (kbd "C-c C-?") 'winner-redo)

(define-key ctl-x-4-map "k" 'my/kill-buffer-other-window)
(define-key ctl-x-4-map "o" 'my/open-buffer-other-window)
(define-key ctl-x-4-map "s" 'my/toggle-split)
(define-key ctl-x-4-map "t" 'my/transpose-windows)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; windows.el ends here
