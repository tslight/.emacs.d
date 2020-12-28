;;; init.el --- init  -*- lexical-binding: t; -*-

;;; Commentary:

;; Copyright: (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(defun my/default-gc-cons-settings ()
  (setq gc-cons-threshold 16777216 ; 16mb
        gc-cons-percentage 0.1))
(add-hook 'emacs-startup-hook 'my/default-gc-cons-settings)

(defvar my/default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun my/restore-default-file-name-handler-alist ()
  (setq file-name-handler-alist my/default-file-name-handler-alist))
(add-hook 'emacs-startup-hook 'my/restore-default-file-name-handler-alist)

(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
(setq auto-save-timeout 5)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t) ;; copy files, don't rename them.
(setq delete-old-versions t)
(setq kept-new-versions 12)
(setq kept-old-versions 12)

(setq ring-bell-function 'ignore)
(setq visible-bell 1)

(setq select-enable-clipboard t)
(setq select-enable-primary t)
(setq x-select-enable-clipboard-manager nil)
(setq save-interprogram-paste-before-kill t)

(setq display-line-numbers 'relative)

(setq-default fill-column 79)
(set-default 'truncate-lines t)

(setq backward-delete-char-untabify-method 'all)

;; https://emacs.stackexchange.com/a/31061
(when (equal system-type 'windows-nt)
  (if (file-readable-p "C:/Program Files/Emacs/x86_64/bin/emacsclient.exe")
      (setq-default with-editor-emacsclient-executable "C:/Program Files/Emacs/x86_64/bin/emacsclient.exe")
    (setq-default with-editor-emacsclient-executable nil)))

(setq create-lockfiles nil) ;; prevent creation of .#myfile.ext

(setq require-final-newline t) ;; useful for crontab

(setq set-mark-command-repeat-pop t) ;; repeating C-SPC after popping, pops it

(setq epa-file-cache-passphrase-for-symmetric-encryption t)
(setf epg-pinentry-mode 'loopback)

(setq history-length t)
(setq history-delete-duplicates t)

(setq bookmark-save-flag 1) ;; always save bookmarks to file

(setq custom-file (make-temp-file "emacs-custom"))

(setq disabled-command-function nil) ;; enable all "advanced" features

(setq message-log-max 10000)

(setq apropos-do-all t) ;; doesn't seem to be documented anywhere..

(setq mouse-yank-at-point t)

(setq scroll-step 4)
(setq scroll-margin 6)
(setq scroll-conservatively 8)
(setq scroll-preserve-screen-position t)

(defun display-startup-echo-area-message ()
  "Redefine this function to be more useful."
  (message "Started in %s. Hacks & Glory await! :-)" (emacs-init-time)))
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'fundamental-mode)

;; (setq password-cache t) ; enable password caching
;; (setq password-cache-expiry 3600) ; for one hour (time in secs)

;; http://www.dr-qubit.org/Lost_undo-tree_history.html
(setq undo-limit 80000000)
(setq undo-strong-limit 90000000)

(setq uniquify-buffer-name-style 'forward)
(setq uniquify-strip-common-suffix t)
(setq uniquify-after-kill-buffer-p t)

(setq user-full-name "Toby Slight")
(setq user-mail-address "tslight@pm.me")

(setq split-width-threshold 160)
(setq split-height-threshold 80)
(setq auto-window-vscroll nil)

(fset 'yes-or-no-p 'y-or-n-p) ;; never have to type full word
(setq confirm-kill-emacs 'y-or-n-p)

(setq c-default-style "bsd")
(setq c-basic-offset 4)
(setq css-indent-offset 2)
(setq js-indent-level 2)

;; If indent-tabs-mode is t, it may use tab, resulting in mixed spaces and tabs
(setq-default indent-tabs-mode nil)

(with-eval-after-load 'python
  (setq python-fill-docstring-style 'django)
  (message "Lazy loaded python :-)"))

;; make tab key do indent first then completion.
(setq-default tab-always-indent 'complete)

;;;###autoload
(defun my/convert-to-unix-coding-system ()
  "Change the current buffer's file encoding to unix."
  (interactive)
  (let ((coding-str (symbol-name buffer-file-coding-system)))
    (when (string-match "-\\(?:dos\\|mac\\)$" coding-str)
      (set-buffer-file-coding-system 'unix))))
(global-set-key (kbd "C-x RET u") 'my/convert-to-unix-coding-system)

;;;###autoload
(defun my/hide-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))
(add-hook 'find-file-hook 'my/hide-dos-eol)

(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(setq coding-system-for-read 'utf-8-unix)
(setq coding-system-for-write 'utf-8-unix)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'lisp-mode-hook 'eldoc-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'text-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)

(autoload 'calculator "calculator" nil t)
(global-set-key (kbd "C-c c") 'calculator)
(autoload 'calc "calc" nil t)
(global-set-key (kbd "C-c M-c") 'calc)

(global-set-key (kbd "C-x M-e") 'eval-buffer)
(global-set-key (kbd "C-x c") 'save-buffers-kill-emacs)
(autoload 'ibuffer "ibuffer" nil t)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x M-k") 'kill-buffer)

(global-set-key (kbd "C-c M-d r") 'desktop-read)
(global-set-key (kbd "C-c M-d s") 'desktop-save)

(global-set-key (kbd "C-c C-e") 'pp-eval-last-sexp)
(global-set-key (kbd "M-;") 'comment-line)
(global-set-key (kbd "C-z") 'zap-up-to-char) ;; suspend is still bound to C-x C-z
(global-set-key (kbd "M-z") 'zap-to-char)
(global-set-key (kbd "C-x M-t") 'transpose-regions)
(global-set-key (kbd "C-x M-p") 'transpose-paragraphs)
(global-set-key (kbd "M-SPC") 'cycle-spacing)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key [remap capitalize-word] 'capitalize-dwim)
(global-set-key [remap downcase-word] 'downcase-dwim)
(global-set-key [remap upcase-word] 'upcase-dwim)

(global-set-key (kbd "C-<f10>") 'toggle-frame-maximized)
(global-set-key (kbd "C-<f11>") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-s-m") 'toggle-frame-maximized)

(autoload 'grep "grep" nil t)
(global-set-key (kbd "C-c C-g") 'grep)

(global-set-key (kbd "C-c M-m") 'menu-bar-mode)
(global-set-key (kbd "S-<f10>") 'menu-bar-mode)

;; for help modes, and simple/special modes
(define-key special-mode-map "n" #'forward-button)
(define-key special-mode-map "p" #'backward-button)
(define-key special-mode-map "f" #'forward-button)
(define-key special-mode-map "b" #'backward-button)
(define-key special-mode-map "n" #'widget-forward)
(define-key special-mode-map "p" #'widget-backward)
(define-key special-mode-map "f" #'widget-forward)
(define-key special-mode-map "b" #'widget-backward)

(global-set-key (kbd "C-c M-t a") 'toggle-text-mode-autofill)
(global-set-key (kbd "C-c M-t t") 'toggle-truncate-lines)

(unless (version< emacs-version "27")
  (global-set-key (kbd "C-x t t") 'tab-bar-select-tab-by-name)
  (global-set-key (kbd "C-x t c") 'tab-bar-new-tab)
  (global-set-key (kbd "C-x t k") 'tab-bar-close-tab)
  (global-set-key (kbd "C-x t n") 'tab-bar-switch-to-next-tab)
  (global-set-key (kbd "C-x t p") 'tab-bar-switch-to-prev-tab)
  (global-set-key (kbd "C-x t l") 'tab-bar-switch-to-recent-tab))

;;;###autoload
(defun my/jump-to-register-other-window ()
  "Tin job."
  (interactive)
  (split-window-sensibly)
  (other-window 1)
  (jump-to-register (register-read-with-preview "Jump to register")))

(global-set-key (kbd "C-x j") 'jump-to-register)
(define-key ctl-x-4-map "j" 'my/jump-to-register-other-window)

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'tooltip-mode) (tooltip-mode -1))
(setq frame-resize-pixelwise t) ;; jwm resize fix

;;;###autoload
(defun my/after-make-frame (frame)
  "Add custom settings after making the FRAME."
  (select-frame frame)
  (if (display-graphic-p)
      (progn
        (when (eq system-type 'windows-nt)
          (set-frame-font "Cascadia Mono 10" nil t))
        (when (eq system-type 'darwin)
          (set-frame-font "Monaco 10" nil t))
        (when (or (eq system-type 'gnu/linux)
                  (eq system-type 'berkeley-unix))
          (set-frame-font "Monospace 11" nil t))
        (if (version< emacs-version "28")
            (load-theme 'wombat)
          (load-theme 'modus-vivendi)))
    (progn
      (if (version< emacs-version "28")
          (load-theme 'manoj-dark)
        (load-theme 'modus-vivendi))
      (xterm-mouse-mode 1)
      (mouse-avoidance-mode 'banish)
      ;; (setq linum-format "%d ")
      (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
      (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
      (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
      (global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 1)))
      (global-set-key [mouse-5] '(lambda () (interactive) (scroll-up 1))))))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'my/after-make-frame(selected-frame))
  (my/after-make-frame(selected-frame)))

(setq display-time-format "%H:%M %d/%m")
(setq display-time-default-load-average 'nil)
(column-number-mode t)
(display-time-mode t)
(display-battery-mode t)
(size-indication-mode t)

(defadvice load-theme (before theme-dont-propagate activate)
  "Disable theme before loading new one."
  (mapc #'disable-theme custom-enabled-themes))

;;;###autoload
(defun my/disable-themes ()
  "Disable all custom themes in one fail swoop."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))
(global-set-key (kbd "C-c M-t C-t") 'my/disable-themes)

(setq default-frame-alist
      '((fullscreen . maximized) (vertical-scroll-bars . nil)))

;;;###autoload
(defun my/indent-buffer ()
  "Indent the contents of a buffer."
  (interactive)
  (indent-region (point-min) (point-max)))
(global-set-key (kbd "M-i") 'my/indent-buffer)

;;;###autoload
(defun my/kill-this-buffer ()
  "Kill the current buffer - `kill-this-buffer' is unreliable."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'my/kill-this-buffer)

;;;###autoload
(defun my/last-buffer ()
  "Switch back and forth between two buffers easily."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "C-c b") 'my/last-buffer)

;;;###autoload
(defun my/nuke-buffers ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapc
   (lambda (buffer)
     (kill-buffer buffer))
   (buffer-list))
  (if current-prefix-arg
      (delete-other-windows)))
(global-set-key (kbd "C-c M-n") 'my/nuke-buffers)

;;;###autoload
(defun my/remove-from-buffer (string)
  "Remove all occurences of STRING from the whole buffer."
  (interactive "sString to remove: ")
  (save-match-data
    (save-excursion
      (let ((count 0))
        (goto-char (point-min))
        (while (re-search-forward string (point-max) t)
          (setq count (+ count 1))
          (replace-match "" nil nil))
        (message (format "%d %s removed from buffer." count string))))))

;;;###autoload
(defun my/remove-character-number (number)
  "Remove all occurences of a control character NUMBER.
  Excluding ^I (tabs) and ^J (newline)."
  (if (and (>= number 0) (<= number 31)
           (not (= number 9)) (not (= number 10)))
      (let ((character (string number)))
        (my/remove-from-buffer character))))

;;;###autoload
(defun my/remove-all-ctrl-characters ()
  "Remove all occurences of all control characters.
  Excluding ^I (tabs) and ^J (newlines)."
  (interactive)
  (mapcar (lambda (n)
            (my/remove-character-number n))
          (number-sequence 0 31)))

;;;###autoload
(defun my/remove-ctrl-m ()
  "Remove all ^M occurrences from EOL in a buffer."
  (interactive)
  (my/remove-from-buffer "$"))
(global-set-key (kbd "C-c k") 'my/remove-from-buffer)

;;;###autoload
(defun my/save-buffers-silently ()
  "Save all open buffers without prompting."
  (interactive)
  (save-some-buffers t)
  (message "Saved all buffers :-)"))
(global-set-key (kbd "C-c s") 'my/save-buffers-silently)

;;;###autoload
(defun my/toggle-maximize-buffer ()
  "Temporarily maximize a buffer."
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))
(global-set-key (kbd "C-c z") 'my/toggle-maximize-buffer)

(setq load-prefer-newer t) ;; if init.elc is older, use newer init.el

(setq compilation-scroll-output 'first-error)

(defun my/ensure-byte-compiled-init ()
  "Run `byte-recompile-file' on config files with 'nil' FORCE and ARG 0.
This means we don't compile if .elc is up to date but we always
create a new .elc file if it doesn't already exist."
  (autoload 'byte-recompile-file "bytecomp")
  (if (file-readable-p (expand-file-name "early-init.el" user-emacs-directory))
      (byte-recompile-file (expand-file-name "early-init.el" user-emacs-directory) 'nil 0))
  (byte-recompile-file (expand-file-name "init.el" user-emacs-directory) 'nil 0))
(add-hook 'after-init-hook 'my/ensure-byte-compiled-init)

(defvar my/files-to-recompile '("early-init.el" "init.el")
  "Files under `user-emacs-directory' that we use for configuration.")

;;;###autoload
(defun my/recompile-config ()
  "Recompile everything in Emacs configuration."
  (interactive)
  (mapc (lambda (file)
          (let ((path (expand-file-name file user-emacs-directory)))
            (when (file-readable-p path)
              (byte-recompile-file path t 0)
              (load (file-name-sans-extension path))
              (message "Re-compiled & loaded %s :-)" path))))
        my/files-to-recompile))

;;;###autoload
(defun my/auto-recompile ()
  "Automatically recompile Emacs Lisp files whenever they are saved."
  (when (or (equal (file-name-extension buffer-file-name) "el")
            (equal major-mode 'emacs-lisp-mode))
    (byte-compile-file buffer-file-name)
    (load (file-name-sans-extension buffer-file-name))
    (message "Re-compiled & loaded %s :-)" buffer-file-name)))
(add-hook 'after-save-hook 'my/auto-recompile)

(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
;;;###autoload
(defun colorize-compilation-buffer ()
  "ANSI color in compilation buffer."
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;;###autoload
(defun my/align-symbol (begin end symbol)
  "Align any SYMBOL in region (between BEGIN and END)."
  (interactive "r\nsEnter align symbol: ")
  (align-regexp begin end (concat "\\(\\s-*\\)" symbol) 1 1))
(global-set-key (kbd "C-c a") 'my/align-symbol)

;;;###autoload
(defun my/align-equals (begin end)
  "Align equals in region (between BEGIN and END)."
  (interactive "r")
  (my/align-symbol begin end "="))
(global-set-key (kbd "C-c =") 'my/align-equals)

;;;###autoload
(defun my/align-colon (begin end)
  "Align colons in region (between BEGIN and END)."
  (interactive "r")
  (my/align-symbol begin end ":"))
(global-set-key (kbd "C-c :") 'my/align-colon)

;;;###autoload
(defun my/align-numbers (begin end)
  "Align numbers in region (between BEGIN and END)."
  (interactive "r")
  (my/align-symbol begin end "[0-9]+"))
(global-set-key (kbd "C-c #") 'my/align-numbers)

(defadvice align-regexp (around align-regexp-with-spaces activate)
  "Force alignment commands to use spaces, not tabs."
  (let ((indent-tabs-mode nil))
    ad-do-it))

;;;###autoload
(defun my/change-number-at-point (change)
  "Change a number by CHANGE amount."
  (let ((number (number-at-point))
        (point (point)))
    (when number
      (progn
        (forward-word)
        (search-backward (number-to-string number))
        (replace-match (number-to-string (funcall change number)))
        (goto-char point)))))

;;;###autoload
(defun my/increment-number-at-point ()
  "Increment number at point."
  (interactive)
  (my/change-number-at-point '1+))
(global-set-key (kbd "C-c +") 'my/increment-number-at-point)

;;;###autoload
(defun my/decrement-number-at-point ()
  "Decrement number at point."
  (interactive)
  (my/change-number-at-point '1-))
(global-set-key (kbd "C-c -") 'my/decrement-number-at-point)

;;;###autoload
(defun my/delete-inside ()
  "Deletes the text within parentheses, brackets or quotes."
  (interactive)
  ;; Search for a match on the same line, don't delete across lines
  (search-backward-regexp "[[{(<\"\']" (line-beginning-position))
  (forward-char)
  (let ((lstart (point)))
    (search-forward-regexp "[]})>\"\']" (line-end-position))
    (backward-char)
    (kill-region lstart (point))))
(global-set-key (kbd "C-c d") 'my/delete-inside)

;;;###autoload
(defun my/generate-numbered-list (start end char)
  "Create a numbered list from START to END.  Using CHAR as punctuation."
  (interactive "nStart number:\nnEnd number:\nsCharacter:")
  (let ((x start))
    (while (<= x end)
      (insert (concat (number-to-string x) char))
      (newline)
      (setq x (+ x 1)))))

;;;###autoload
(defun smart/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'smart/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))
(global-set-key [remap fill-paragraph] 'smart/fill-or-unfill)

;;;###autoload
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
(define-key ctl-x-map "n" 'smart/narrow-or-widen-dwim)

;;;###autoload
(defun smart/move-beginning-of-line ()
  "Move point back to indentation.

If there is any non blank characters to the left of the cursor.
Otherwise point moves to beginning of line."
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))
(global-set-key [remap move-beginning-of-line] 'smart/move-beginning-of-line)

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun my/sort-lines-nocase ()
  "Sort marked lines with case sensitivity."
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))

;;;###autoload
(defun my/surround (begin end open close)
  "Put OPEN at BEGIN and CLOSE at END of the region.

If you omit CLOSE, it will reuse OPEN."
  (interactive  "r\nsStart: \nsEnd: ")
  (save-excursion
    (goto-char end)
    (if (string= close "")
        (insert open)
      (insert close))
    (goto-char begin)
    (insert open)))
(global-set-key (kbd "M-s M-s") 'my/surround)

;;;###autoload
(defun my/untabify-buffer ()
  "Convert all tabs to spaces in the buffer."
  (interactive)
  (untabify (point-min) (point-max)))

;;;###autoload
(defun my/xml-pretty-print ()
  "Reformat and indent XML."
  (interactive)
  (save-excursion
    (sgml-pretty-print (point-min) (point-max))
    (indent-region (point-min) (point-max))))

;;;###autoload
(defun my/yank-pop-forwards (arg)
  "Cycle forwards through the kill.  Reverse `yank-pop'.  With ARG."
  (interactive "p")
  (yank-pop (- arg)))
(global-set-key (kbd "C-M-y") 'my/yank-pop-forwards)

;;;###autoload
(defun my/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))
(global-set-key (kbd "C-c f d") 'my/delete-this-file)

;;;###autoload
(defun my/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))
(global-set-key (kbd "C-c f w") 'my/copy-file-name-to-clipboard)

;;;###autoload
(defun my/make-backup ()
  "Make a backup copy of current file or dired marked files.

If in dired, backup current file or marked files."
  (interactive)
  (let (($fname (buffer-file-name)))
    (if $fname
        (let (($backup-name
               (concat $fname "." (format-time-string "%y%m%d%H%M") ".bak")))
          (copy-file $fname $backup-name t)
          (message (concat "Backup saved at: " $backup-name)))
      (if (string-equal major-mode "dired-mode")
          (progn
            (mapc (lambda ($x)
                    (let (($backup-name
                           (concat $x "." (format-time-string "%y%m%d%H%M") ".bak")))
                      (copy-file $x $backup-name t)))
                  (dired-get-marked-files))
            (message "marked files backed up"))
        (user-error "Buffer not file nor dired")))))

;;;###autoload
(defun my/make-backup-and-save ()
  "Backup of current file and save, or backup dired marked files.
For detail, see `my/make-backup'."
  (interactive)
  (if (buffer-file-name)
      (progn
        (my/make-backup)
        (when (buffer-modified-p)
          (save-buffer)))
    (progn
      (my/make-backup))))
(global-set-key (kbd "C-c f b") 'my/make-backup-and-save)

;;;###autoload
(defun my/rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "FNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (when (file-exists-p filename)
          (rename-file filename new-name 1))
        (rename-buffer new-name)
        (set-visited-file-name new-name)))))
(global-set-key (kbd "C-c f r") 'my/rename-this-file-and-buffer)

;;;###autoload
(defun my/sudoedit (&optional arg)
  "Open current or ARG file as root."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (read-file-name "Find file (as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
(global-set-key (kbd "C-c f s") 'my/sudoedit)

;;;###autoload
(defun my/google (arg)
  "Googles a query or region.  With prefix ARG, wrap in quotes."
  (interactive "P")
  (let ((query
         (if (region-active-p)
             (buffer-substring (region-beginning) (region-end))
           (read-string "Query: "))))
    (when arg (setq query (concat "\"" query "\"")))
    (browse-url
     (concat "http://www.google.com/search?ie=utf-8&oe=utf-8&q=" query))))
(global-set-key (kbd "C-c M-g") 'my/google)

;;;###autoload
(defmacro my/measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defun my/scroll-other-window (arg)
  "Scroll up other window when called with prefix."
  (interactive "P")
  (if arg (scroll-other-window-down) (scroll-other-window)))

(global-set-key [remap scroll-other-window] 'my/scroll-other-window)

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
(define-key ctl-x-4-map "k" 'my/kill-buffer-other-window)

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
(global-set-key (kbd "C-c w w") 'my/last-window)

;;;###autoload
(defun my/open-buffer-other-window (buffer)
  "Open a BUFFER in another window without switching to it."
  (interactive "BBuffer: ")
  (switch-to-buffer-other-window buffer)
  (other-window -1))
(define-key ctl-x-4-map "o" 'my/open-buffer-other-window)

;;;###autoload
(defun my/prev-window ()
  "Go the previously used window, excluding other frames."
  (interactive)
  (other-window -1))
(global-set-key (kbd "C-x O") 'my/prev-window)

;;;###autoload
(defun my/scroll-line-up (n)
  "Scroll line up N lines.  Like Ctrl-e in Vim."
  (interactive "p")
  (scroll-up n))
(global-set-key (kbd "M-p") 'my/scroll-line-up)

;;;###autoload
(defun my/scroll-line-down (n)
  "Scroll line down N lines.  Ctrl-y in Vim."
  (interactive "p")
  (scroll-down n))
(global-set-key (kbd "M-n") 'my/scroll-line-down)

;;;###autoload
(defun my/hsplit-last-buffer (prefix)
  "Split the window vertically and display the previous buffer.
With PREFIX stay in current buffer."
  (interactive "p")
  (split-window-vertically)
  (other-window 1 nil)
  (if (= prefix 1)
      (switch-to-next-buffer)))
(global-set-key (kbd "C-c 2") 'my/hsplit-last-buffer)

;;;###autoload
(defun my/vsplit-last-buffer (prefix)
  "Split the window horizontally and display the previous buffer.
With PREFIX stay in current buffer."
  (interactive "p")
  (split-window-horizontally)
  (other-window 1 nil)
  (if (= prefix 1) (switch-to-next-buffer)))
(global-set-key (kbd "C-c 3") 'my/vsplit-last-buffer)

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
(define-key ctl-x-4-map "s" 'my/toggle-split)

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
(define-key ctl-x-4-map "t" 'my/transpose-windows)

(autoload 'windmove-left "windmove" nil t)
(global-set-key (kbd "C-c w b") 'windmove-left)
(autoload 'windmove-right "windmove" nil t)
(global-set-key (kbd "C-c w f") 'windmove-right)
(autoload 'windmove-up "windmove" nil t)
(global-set-key (kbd "C-c w p") 'windmove-up)
(autoload 'windmove-down "windmove" nil t)
(global-set-key (kbd "C-c w n") 'windmove-down)
(with-eval-after-load 'windmove
  (setq windmove-wrap-around t)
  (message "Lazy loaded windmove :-)"))

(add-hook 'window-setup-hook 'winner-mode)
(global-set-key (kbd "C-c w u") 'winner-undo)
(global-set-key (kbd "C-c w r") 'winner-redo)

(add-hook 'after-init-hook 'global-auto-revert-mode) ;; reload if file changed on disk

(with-eval-after-load 'dabbrev
  (setq abbrev-file-name (concat user-emacs-directory "abbrevs"))
  (setq save-abbrevs 'silently)
  (unless (version< emacs-version "28")
    (setq abbrev-suggest t))
  (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
  (setq dabbrev-abbrev-skip-leading-regexp "[$*/=']")
  (setq dabbrev-backward-only nil)
  (setq dabbrev-case-distinction 'case-replace)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-case-replace 'case-replace)
  (setq dabbrev-check-other-buffers t)
  (setq dabbrev-eliminate-newlines t)
  (setq dabbrev-upcase-means-case-search t)
  (message "Lazy loaded dabbrev :-)"))

(with-eval-after-load 'dired
;;;###autoload
  (defun my/dired-get-size ()
    "Get cumlative size of marked or current item."
    (interactive)
    (let ((files (dired-get-marked-files)))
      (with-temp-buffer
        (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
        (message "Size of all marked files: %s"
                 (progn
                   (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
                   (match-string 1))))))
  (define-key dired-mode-map "?" 'my/dired-get-size)

;;;###autoload
  (defun my/dired-beginning-of-buffer ()
    "Go to first file in directory."
    (interactive)
    (goto-char (point-min))
    (dired-next-line 2))
  (define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'my/dired-beginning-of-buffer)

;;;###autoload
  (defun my/dired-end-of-buffer ()
    "Go to last file in directory."
    (interactive)
    (goto-char (point-max))
    (dired-next-line -1))
  (define-key dired-mode-map (vector 'remap 'end-of-buffer) 'my/dired-end-of-buffer)

  (defvar dired-compress-files-alist
    '(("\\.tar\\.gz\\'" . "tar -c %i | gzip -c9 > %o")
      ("\\.zip\\'" . "zip %o -r --filesync %i"))
    "Control the compression shell command for `dired-do-compress-to'.

  Each element is (REGEXP . CMD), where REGEXP is the name of the
  archive to which you want to compress, and CMD the the
  corresponding command.

  Within CMD, %i denotes the input file(s), and %o denotes the
  output file.  %i path(s) are relative, while %o is absolute.")

  (autoload 'dired-omit-mode "dired-x"
    "Omit files from dired listings." t)

  (autoload 'dired-omit-files "dired-x"
    "User regex to specify what files to omit." t)
  (setq dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'\\|^\\..+$")

  (when (eq system-type 'berkeley-unix)
    (setq dired-listing-switches "-alhpL"))

  (when (eq system-type 'gnu/linux)
    (setq dired-listing-switches
          "-AGFhlv --group-directories-first --time-style=long-iso"))

  (setq dired-dwim-target t
        delete-by-moving-to-trash t
        dired-use-ls-dired nil
        dired-recursive-copies 'always
        dired-recursive-deletes 'always)

  (defun my/dired-up-directory ()
    (interactive)
    (find-alternate-file ".."))
  (define-key dired-mode-map "b" 'my/dired-up-directory)

  (define-key dired-mode-map "f" 'dired-find-alternate-file)
  (define-key dired-mode-map "c" 'dired-do-compress-to)
  (define-key dired-mode-map ")" 'dired-omit-mode)
  (message "Lazy loaded dired :-)"))

;; This is in `dired' not `dired-jump' in Emacs 28
(when (version< emacs-version "28")
  (autoload 'dired-jump "dired-x" nil t)
  (global-set-key (kbd "C-x C-j") 'dired-jump)
  (autoload 'dired-jump-other-window "dired-x" nil t)
  (define-key ctl-x-4-map "C-j" 'dired-jump-other-window))

(add-hook 'dired-mode-hook 'hl-line-mode)

(with-eval-after-load 'dired-aux
  (setq dired-isearch-filenames 'dwim)
  ;; The following variables were introduced in Emacs 27.1
  (unless (version< emacs-version "27.1")
    (setq dired-create-destination-dirs 'ask)
    (setq dired-vc-rename-file t))
  (message "Lazy loaded dired-aux :-)"))

(with-eval-after-load 'find-dired
  ;; (setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))
  (setq find-ls-option
        '("-ls" . "-AGFhlv --group-directories-first --time-style=long-iso"))
  (setq find-name-arg "-iname")
  (message "Lazy loaded find-dired :-)"))

(with-eval-after-load 'wdired
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t)
  (message "Lazy loaded wdired :-)"))

(with-eval-after-load 'doc-view-mode
  (setq doc-view-continuous t)
  (setq doc-view-resolution 300)
  (message "Lazy loaded doc-view-mode :-)"))

(with-eval-after-load 'ediff
  (setq ediff-diff-options "-w")
  (setq ediff-keep-variants nil)
  (setq ediff-make-buffers-readonly-at-startup nil)
  (setq ediff-merge-revisions-with-ancestor t)
  (setq ediff-show-clashes-only t)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo)

  ;; https://emacs.stackexchange.com/a/24602
  ;;;###autoload
  (defun disable-y-or-n-p (orig-fun &rest args)
    "Advise ORIG-FUN with ARGS so it dynamically rebinds `y-or-n-p'."
    (cl-letf (((symbol-function 'y-or-n-p) (lambda () t)))
      (apply orig-fun args)))

  (advice-add 'ediff-quit :around #'disable-y-or-n-p)
  (message "Lazy loaded ediff :-)"))

(add-hook 'after-init-hook 'electric-indent-mode)
(add-hook 'after-init-hook 'electric-pair-mode)

(with-eval-after-load 'erc
  (setq erc-autojoin-channels-alist '(("freenode.net"
                                       "#org-mode"
                                       "#emacs")))
  (setq erc-fill-column 80)
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))
  (setq erc-input-line-position -2)
  (setq erc-keywords '("not2b"))
  (setq erc-nick "not2b")
  (setq erc-prompt-for-password t)
  (setq erc-track-enable-keybindings t)
  (message "Lazy loaded erc :-)"))

(with-eval-after-load 'eshell
;;;###autoload
  (defun my/eshell-complete-recent-dir (&optional arg)
    "Switch to a recent `eshell' directory using completion.
With \\[universal-argument] also open the directory in a `dired'
buffer."
    (interactive "P")
    (let* ((dirs (ring-elements eshell-last-dir-ring))
           (dir (completing-read "Switch to recent dir: " dirs nil t)))
      (insert dir)
      (eshell-send-input)
      (when arg
        (dired dir))))

;;;###autoload
  (defun my/eshell-complete-history ()
    "Insert element from `eshell' history using completion."
    (interactive)
    (let ((hist (ring-elements eshell-history-ring)))
      (insert
       (completing-read "Input history: " hist nil t))))

;;;###autoload
  (defun my/eshell-prompt ()
    "Custom eshell prompt."
    (concat
     (propertize (user-login-name) 'face `(:foreground "green" ))
     (propertize "@" 'face `(:foreground "yellow"))
     (propertize (system-name) `face `(:foreground "green"))
     (propertize ":" 'face `(:foreground "yellow"))
     (if (string= (eshell/pwd) (getenv "HOME"))
         (propertize "~" 'face `(:foreground "magenta"))
       (propertize (eshell/basename (eshell/pwd)) 'face `(:foreground "magenta")))
     (propertize (ignore-errors (format " (%s)"
                                        (vc-responsible-backend default-directory)))
                 'face `(:foreground "cyan"))
     "\n"
     (if (= (user-uid) 0)
         (propertize "#" 'face `(:foreground "red"))
       (propertize "$" 'face `(:foreground "yellow")))
     (propertize " " 'face `(:foreground "white"))))

  ;; https://www.emacswiki.org/emacs/EshellPrompt
  (setq
   eshell-cd-on-directory t
   eshell-destroy-buffer-when-process-dies t
   eshell-highlight-prompt nil
   eshell-hist-ignoredups t
   eshell-history-size 4096
   eshell-ls-use-colors t
   eshell-prefer-lisp-functions t
   eshell-prefer-lisp-variables t
   eshell-prompt-regexp "^[^#$\n]*[#$] "
   eshell-prompt-function 'my/eshell-prompt
   eshell-review-quick-commands nil
   eshell-save-history-on-exit t
   eshell-smart-space-goes-to-end t
   eshell-where-to-jump 'begin)

  (add-to-list 'eshell-modules-list 'eshell-tramp) ;; no sudo password with ~/.authinfo
  (add-hook 'eshell-preoutput-filter-functions 'ansi-color-apply)

  (defun my/eshell-keys()
    (define-key eshell-mode-map (kbd "M-r") 'my/eshell-complete-history)
    (define-key eshell-mode-map (kbd "C-=") 'my/eshell-complete-recent-dir))

  (add-hook 'eshell-mode-hook 'my/eshell-keys)
  (message "Lazy loaded eshell :-)"))

;;;###autoload
(defun my/eshell-other-window ()
  "Open an `eshell' in another window."
  (interactive)
  (split-window-sensibly)
  (other-window 1)
  (eshell))

(autoload 'eshell "eshell" nil t)
(global-set-key (kbd "C-c e") 'eshell)
(global-set-key (kbd "C-c 4 e") 'my/eshell-other-window)

(with-eval-after-load 'gnus
  (require 'nnir)
  (setq gnus-init-file "~/.emacs.d/init.el")
  (setq gnus-home-directory "~/.emacs.d/")
  (setq message-directory "~/.emacs.d/mail")
  (setq gnus-directory "~/.emacs.d/news")
  (setq nnfolder-directory "~/.emacs.d/mail/archive")
  (setq gnus-use-full-window nil)
  (setq gnus-select-method '(nntp "news.gwene.org"))
  ;; (setq gnus-secondary-select-methods '((nntp "news.gnus.org")))
  (setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)
  (setq gnus-thread-hide-subtree t)
  (setq gnus-thread-ignore-subject t)
  (message "Lazy loaded gnus :-)"))

(setq highlight-changes-visibility-initial-state nil)
(global-set-key (kbd "C-c h n") 'highlight-changes-next-change)
(global-set-key (kbd "C-c h p") 'highlight-changes-previous-change)
(add-hook 'emacs-startup-hook 'global-highlight-changes-mode)

;;;###autoload
(defun my/hippie-expand-completions (&optional hippie-expand-function)
  "Return the full list of completions generated by HIPPIE-EXPAND-FUNCTION.
The optional argument can be generated with `make-hippie-expand-function'."
  (let ((this-command 'my/hippie-expand-completions)
        (last-command last-command)
        (buffer-modified (buffer-modified-p))
        (hippie-expand-function (or hippie-expand-function 'hippie-expand)))
    (cl-flet ((ding)) ; avoid the (ding) when hippie-expand exhausts its options.
      (while (progn
               (funcall hippie-expand-function nil)
               (setq last-command 'my/hippie-expand-completions)
               (not (equal he-num -1)))))
    ;; Evaluating the completions modifies the buffer, however we will finish
    ;; up in the same state that we began.
    (set-buffer-modified-p buffer-modified)
    ;; Provide the options in the order in which they are normally generated.
    (delete he-search-string (reverse he-tried-table))))

;;;###autoload
(defun my/hippie-complete-with (hippie-expand-function)
  "Offer `completing-read' using the specified HIPPIE-EXPAND-FUNCTION."
  (let* ((options (my/hippie-expand-completions hippie-expand-function))
         (selection (and options (completing-read "Completions: " options))))
    (if selection
        (he-substitute-string selection t)
      (message "No expansion found"))))

;;;###autoload
(defun my/hippie-expand-completing-read ()
  "Offer `completing-read' for the word at point."
  (interactive)
  (my/hippie-complete-with 'hippie-expand))
(global-set-key (kbd "C-c /") 'my/hippie-expand-completing-read)

(global-set-key (kbd "M-/") 'hippie-expand)

(if (version< emacs-version "27")
    (icomplete-mode)
  (fido-mode))

;;;###autoload
(defun my/icomplete-styles ()
  "Set icomplete styles based on Emacs version."
  (if (version< emacs-version "27")
      (setq-local completion-styles '(initials partial-completion substring basic))
    (setq-local completion-styles '(initials partial-completion flex substring basic))))
(add-hook 'icomplete-minibuffer-setup-hook 'my/icomplete-styles)

(setq icomplete-delay-completions-threshold 100)
(setq icomplete-max-delay-chars 2)
(setq icomplete-compute-delay 0.2)
(setq icomplete-show-matches-on-no-input t)
(setq icomplete-hide-common-prefix nil)
(setq icomplete-prospects-height 1)
;; (setq icomplete-separator "\n")
(setq icomplete-separator (propertize " · " 'face 'shadow))
(setq icomplete-with-completion-tables t)
(setq icomplete-tidy-shadowed-file-names t)
(setq icomplete-in-buffer t)

(unless (version< emacs-version "27")
    (define-key icomplete-minibuffer-map (kbd "C-j") 'icomplete-fido-exit))
(define-key icomplete-minibuffer-map (kbd "M-j") 'exit-minibuffer)
(define-key icomplete-minibuffer-map (kbd "C-n") 'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "C-p") 'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "<up>") 'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "<down>") 'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "<left>") 'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "<right>") 'icomplete-forward-completions)

(with-eval-after-load 'imenu
  (setq imenu-auto-rescan t)
  (setq imenu-auto-rescan-maxout 600000)
  (setq imenu-eager-completion-buffer t)
  (setq imenu-level-separator "/")
  (setq imenu-max-item-length 100)
  (setq imenu-space-replacement " ")
  (setq imenu-use-markers t)
  (setq imenu-use-popup-menu nil)
  (message "Lazy loaded imenu :-)"))

(autoload 'imenu "imenu" nil t)
(global-set-key (kbd "C-c i") 'imenu)

(with-eval-after-load 'isearch
   ;;;###autoload
  (defun my/isearch-exit ()
    "Move point to the start of the matched string."
    (interactive)
    (when (eq isearch-forward t)
      (goto-char isearch-other-end))
    (isearch-exit))

  ;;;###autoload
  (defun my/isearch-abort-dwim ()
    "Delete failed `isearch' input, single char, or cancel search.

This is a modified variant of `isearch-abort' that allows us to
perform the following, based on the specifics of the case: (i)
delete the entirety of a non-matching part, when present; (ii)
delete a single character, when possible; (iii) exit current
search if no character is present and go back to point where the
search started."
    (interactive)
    (if (eq (length isearch-string) 0)
        (isearch-cancel)
      (isearch-del-char)
      (while (or (not isearch-success) isearch-error)
        (isearch-pop-state)))
    (isearch-update))

  ;;;###autoload
  (defun my/copy-to-isearch ()
    "Copy up to the search match when searching forward.

When searching backward, copy to the start of the search match."
    (interactive)
    (my/isearch-exit)
    (call-interactively 'kill-ring-save)
    (exchange-point-and-mark))

  ;;;###autoload
  (defun my/kill-to-isearch ()
    "Kill up to the search match when searching forward.

When searching backward, kill to the beginning of the match."
    (interactive)
    (my/isearch-exit)
    (call-interactively 'kill-region))

  (unless (version< emacs-version "27.1")
    (setq isearch-allow-scroll 'unlimited)
    (setq isearch-yank-on-move 't)
    (setq isearch-lazy-count t)
    (setq lazy-count-prefix-format nil)
    (setq lazy-count-suffix-format " (%s/%s)"))
  (setq search-highlight t)
  (setq search-whitespace-regexp ".*?")
  (setq isearch-lax-whitespace t)
  (setq isearch-regexp-lax-whitespace nil)
  (setq isearch-lazy-highlight t)

  (define-key isearch-mode-map (kbd "RET") 'my/isearch-exit)
  (define-key isearch-mode-map (kbd "<backspace>") 'my/isearch-abort-dwim)
  (define-key isearch-mode-map (kbd "M-w") 'my/copy-to-isearch)
  (define-key isearch-mode-map (kbd "C-M-w") 'my/kill-to-isearch)
  (define-key isearch-mode-map (kbd "M-/") 'isearch-complete)
  (define-key minibuffer-local-isearch-map (kbd "M-/") 'isearch-complete-edit)
  (message "Lazy loaded isearch :-)"))

(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "M-s b") 'multi-isearch-buffers-regexp)
(global-set-key (kbd "M-s f") 'multi-isearch-files-regexp)
(global-set-key (kbd "M-s M-o") 'multi-occur)

(add-hook 'occur-mode-hook 'hl-line-mode)
(define-key occur-mode-map "t" 'toggle-truncate-lines)

(with-eval-after-load 'savehist
  (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (setq savehist-save-minibuffer-history 1)
  (message "Lazy loaded savehist :-)"))
(add-hook 'after-init-hook 'savehist-mode)

(setq completion-category-defaults nil)
(setq completion-cycle-threshold 3)
(setq completion-flex-nospace nil)
(setq completion-ignore-case t)
(setq completion-pcm-complete-word-inserts-delimiters t)
(setq completion-pcm-word-delimiters "-_./:| ")
(setq completion-show-help nil)
(setq completions-detailed t)
(setq completions-format 'one-column)

(setq enable-recursive-minibuffers t)
(setq file-name-shadow-mode 1)
(setq minibuffer-depth-indicate-mode 1)
(setq minibuffer-eldef-shorten-default t)
(setq minibuffer-electric-default-mode 1)
(setq read-answer-short t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq resize-mini-windows t)

(add-hook 'minibuffer-setup-hook (lambda () (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook (lambda () (setq gc-cons-threshold 16777216))) ; 16mb

(with-eval-after-load 'org
  (require 'org-tempo)
;;;###autoload
  (defun my/org-recursive-sort ()
    "Sort all entries in the current buffer, recursively."
    (interactive)
    (org-map-entries
     (lambda ()
       (condition-case x
           (org-sort-entries nil ?a)
         (user-error)))))

;;;###autoload
  (defun my/org-tangle-block ()
    "Only tangle the block at point."
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'org-babel-tangle)))

;;;###autoload
  (defun my/org-babel-insert-elisp-boilerplate (file)
    "Insert Emacs Lisp documentation comments into FILE and add lexical binding."
    (when (equal (file-name-extension file) "el")
      (with-current-buffer (find-file-noselect file)
        (let* ((filename (file-name-sans-extension (file-name-nondirectory file)))
               (copyright (concat (format-time-string "%Y") " " user-full-name))
               (author (concat user-full-name " <"user-mail-address">"))
               (header (concat ";;; " filename ".el --- " filename"\n\n"
                               ";;; Commentary:\n\n"
                               ";; Copyright: (C) " copyright "\n"
                               ";; Author: " author "\n\n"
                               ";;; Code:\n"))
               (footer (concat "\n(provide '" filename ")\n"
                               ";; Local Variables:\n"
                               ";; indent-tabs-mode: nil\n"
                               ";; byte-compile-warnings: (not free-vars noruntime)\n"
                               ";; End:\n"
                               ";;; " filename ".el ends here")))
          (goto-char (point-min)) (insert header)
          (message "Inserted header comments into %s" file)
          (goto-char (point-max)) (insert footer)
          (message "Inserted footer comments into %s" file)
          (add-file-local-variable-prop-line 'lexical-binding t)
          (message "Added lexical binding to %s" file)
          (save-buffer)
          (kill-buffer)
          (message "Saved %s :-)" file)))
      (when (file-readable-p (concat file "~"))
        (delete-file (concat file "~"))
        (message "Deleted %s~ backup file!" %s))))

  (add-hook 'org-babel-post-tangle-hook
            (lambda () (my/org-babel-insert-elisp-boilerplate buffer-file-name)))

;;;###autoload
  (defun my/buffer-substring-p (string)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (if (re-search-forward string nil t) t nil))))

;;;###autoload
  (defun my/org-babel-auto-tangle-init-file ()
    (if (and (string-match "^.*README\\.org$" (buffer-file-name))
             (my/buffer-substring-p
              "^\\#\\+PROPERTY\\: header-args\\+ \\:tangle \\~\\/\\.emacs.d\\/init\\.el"))
        (org-babel-tangle)))
  (add-hook 'after-save-hook 'my/org-babel-auto-tangle-init-file)

  (setq org-image-actual-width nil)
  (setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  (setq org-emphasis-regexp-components '(" \t('\"{" "- \t.,:!?;'\")}\\" " \t\r\n,\"'" "." 300))
  (setq org-confirm-babel-evaluate t)
  (setq org-agenda-files (file-expand-wildcards "~/*.org"))
  (setq org-agenda-files (quote ("~/org/todo.org")))
  (setq org-default-notes-file "~/org/notes.org")
  (setq org-directory "~/org")
  (setq org-export-with-toc t)
  (setq org-indent-indentation-per-level 1)
  (setq org-list-allow-alphabetical t)
  (setq org-list-indent-offset 1)
  ;; (setq org-replace-disputed-keys t) ;; fix windmove conflicts
  (setq org-return-follows-link t)
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets '((nil :maxlevel . 9)))
  (setq org-speed-commands-user (quote (("N" . org-down-element)
                                        ("P" . org-up-element))))
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-window-setup 'current-window)
  (setq org-startup-indented t)
  (setq org-use-fast-todo-selection t)
  (setq org-use-speed-commands t)

  (setq org-latex-listings 'minted)
  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (add-to-list 'org-latex-packages-alist '("" "minted"))

  (setq org-capture-templates
        '(("t" "TODO Entry" entry (file+headline "~/org/todo.org" "CURRENT")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal Entry" entry (file+datetree "~/org/journal.org" "JOURNAL")
           "* %?\nEntered on %U\n  %i\n  %a")))

  (add-to-list 'org-structure-template-alist '("cl" . "src common-lisp"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("go" . "src go"))
  (add-to-list 'org-structure-template-alist '("ja" . "src java"))
  (add-to-list 'org-structure-template-alist '("js" . "src javascript"))
  (add-to-list 'org-structure-template-alist '("kr" . "src c"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("sq" . "src sql"))
  (add-to-list 'org-structure-template-alist '("tx" . "src text"))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((awk . t)
     (C . t)
     (clojure . t)
     (css . t)
     (dot . t) ;; graphviz language
     (emacs-lisp . t)
     (gnuplot . t)
     (haskell . t)
     ;; (http . t)
     (java . t)
     (js . t)
     (latex . t)
     (lisp . t)
     (makefile . t)
     (ocaml . t)
     (perl . t)
     (python . t)
     (plantuml . t)
     (ruby . t)
     (scheme . t)
     (sed . t)
     (shell . t)
     (sql . t)
     (sqlite . t)))

  (add-hook 'org-mode-hook 'auto-fill-mode)
  (add-hook 'org-mode-hook 'hl-line-mode)
  (message "Lazy loaded org :-)"))

(add-hook 'after-init-hook 'pending-delete-mode 1) ;; remove selected region if typing

(with-eval-after-load 'prettify-symbols
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  (message "Lazy loaded prettify-symbols :-)"))
(add-hook 'emacs-startup-hook 'global-prettify-symbols-mode)

(unless (version< emacs-version "28")
  (setq my/project-roots '("~" "~/src/gitlab"))

;;;###autoload
  (defun my/project--git-repo-p (directory)
    "Return non-nil if there is a git repository in DIRECTORY."
    (and
     (file-directory-p (concat directory "/.git"))
     (file-directory-p (concat directory "/.git/info"))
     (file-directory-p (concat directory "/.git/objects"))
     (file-directory-p (concat directory "/.git/refs"))
     (file-regular-p (concat directory "/.git/HEAD"))))

;;;###autoload
  (defun my/project--git-repos-recursive (directory maxdepth)
    "List git repos in under DIRECTORY recursively to MAXDEPTH."
    (let* ((git-repos '())
           (current-directory-list
            (directory-files directory t directory-files-no-dot-files-regexp)))
      ;; while we are in the current directory
      (if (my/project--git-repo-p directory)
          (setq git-repos (cons (file-truename (expand-file-name directory)) git-repos)))
      (while current-directory-list
        (let ((f (car current-directory-list)))
          (cond ((and (file-directory-p f)
                      (file-readable-p f)
                      (> maxdepth 0)
                      (not (my/project--git-repo-p f)))
                 (setq git-repos
                       (append git-repos
                               (my/project--git-repos-recursive f (- maxdepth 1)))))
                ((my/project--git-repo-p f)
                 (setq git-repos (cons
                                  (file-truename (expand-file-name f)) git-repos))))
          (setq current-directory-list (cdr current-directory-list))))
      (delete-dups git-repos)))

;;;###autoload
  (defun my/project--list-projects ()
    "Produce list of projects in `my/project-roots'."
    (let ((cands (delete-dups (mapcan (lambda (directory)
                                        (my/project--git-repos-recursive
                                         (expand-file-name directory)
                                         10))
                                      my/project-roots))))
      ;; needs to be a list of lists
      (mapcar (lambda (d)
                (list (abbreviate-file-name d)))
              cands)))

;;;###autoload
  (defun my/project-update-projects ()
    "Overwrite `project--list' using `my/project--list-projects'.
    WARNING: This will destroy & replace the contents of `project-list-file'."
    (interactive)
    (autoload 'project--ensure-read-project-list "project" nil t)
    (project--ensure-read-project-list)
    (setq project--list (my/project--list-projects))
    (project--write-project-list)
    (message "Updated project list in %s" project-list-file))

  ;; (add-hook 'emacs-startup-hook 'my/project-update-projects)
  (global-set-key (kbd "C-x p u") 'my/project-update-projects)

  (with-eval-after-load 'project
    (setq project-switch-commands
          '((?b "Buffer" project-switch-to-buffer)
            (?c "Compile" project-compile)
            (?d "Dired" project-dired)
            (?e "Eshell" project-eshell)
            (?f "File" project-find-file)
            (?g "Grep" project-find-regexp)
            (?q "Query replace" project-query-replace-regexp)
            (?r "Run command" project-async-shell-command)
            (?s "Search" project-search)
            (?v "VC dir" project-vc-dir)))
    (message "Lazy loaded project :-)")))

(with-eval-after-load 'recentf
  (setq recentf-exclude '(".gz"
                          ".xz"
                          ".zip"
                          "/elpa/"
                          "/ssh:"
                          "/sudo:"
                          "^/var/folders\\.*"
                          "COMMIT_EDITMSG\\'"
                          ".*-autoloads\\.el\\'"
                          "[/\\]\\.elpa/"))
  (setq recentf-max-menu-items 128)
  (setq recentf-max-saved-items 256)

  ;;;###autoload
  (defun my/completing-recentf ()
    "Show a list of recent files."
    (interactive)
    (let* ((all-files recentf-list)
           (list1 (mapcar (lambda (x) (file-name-nondirectory x) x) all-files))
           (list2 (mapcar #'substring-no-properties list1))
           (list3 (mapcar #'abbreviate-file-name list2))
           (list4 (cl-remove-duplicates list3 :test #'string-equal)))
      (find-file (completing-read "Recent Files: " list4 nil t))))
  (global-set-key (kbd "C-c r") 'my/completing-recentf)

  (defun my/completing-recentf-other-window ()
    (interactive)
    (split-window-sensibly)
    (other-window 1)
    (my/completing-recentf))
  (global-set-key (kbd "C-c 4 r") 'my/completing-recentf-other-window)

  (message "Lazy loaded recentf :-)"))

(global-set-key (kbd "C-c C-r") 'recentf-open-files)
(add-hook 'after-init-hook 'recentf-mode)

(add-hook 'after-init-hook 'show-paren-mode)

(with-eval-after-load 'save-place
  (setq save-place-file (concat user-emacs-directory "saveplace.el"))
  (message "Lazy loaded save-place-mode :-)"))
(add-hook 'emacs-startup-hook 'save-place-mode)

(with-eval-after-load 'sh-script
  (add-hook 'shell-script-mode-hook 'hl-line-mode)
  (add-hook 'sh-script-hook 'display-line-numbers-mode)
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
  (message "Lazy loaded shell-script-mode :-)"))

(add-hook 'after-init-hook 'global-subword-mode) ;; move by camel case, etc

(autoload 'term "term" nil t)
(autoload 'ansi-term "term" nil t)

;;;###autoload
(defun my/ansi-term ()
  "Opens shell from $SHELL environmental variable in `ansi-term'."
  (interactive)
  ;; https://emacs.stackexchange.com/a/48481
  (let ((switch-to-buffer-obey-display-actions))
    (ansi-term (getenv "SHELL"))))
(global-set-key (kbd "C-c tt") 'my/ansi-term)

;;;###autoload
(defun my/ansi-term-other-window ()
  "Opens default $SHELL `ansi-term' in another window."
  (interactive)
  (split-window-sensibly)
  (other-window 1)
  (my/ansi-term))
(global-set-key (kbd "C-c 4 tt") 'my/ansi-term-other-window)

;;;###autoload
(defun my/switch-to-ansi-term ()
  "Open an `ansi-term' if it doesn't already exist.
Otherwise switch to current one."
  (interactive)
  (if (get-buffer "*ansi-term*")
      (switch-to-buffer "*ansi-term*")
    (ansi-term (getenv "SHELL"))))
(global-set-key (kbd "C-c ts") 'my/switch-to-ansi-term)

;;;###autoload
(defun my/switch-to-ansi-term-other-window()
  "Does what it states on the tin!"
  (interactive)
  (split-window-sensibly)
  (other-window 1)
  (my/switch-to-ansi-term))
(global-set-key (kbd "C-c 4 ts") 'my/switch-to-ansi-term-other-window)

(with-eval-after-load 'term
  (defadvice term-handle-exit (after term-kill-buffer-on-exit activate)
    "Kill term when shell exits."
    (kill-buffer))
  (setq term-buffer-maximum-size 200000)
  (message "Lazy loaded term :-)"))

(add-hook 'term-exec (lambda () (set-process-coding-system 'utf-8-unix 'utf-8-unix)))

(with-eval-after-load 'tramp
  (setq tramp-backup-directory-alist backup-directory-alist)
  (setq tramp-default-method "ssh")
  (setf tramp-persistency-file-name (concat temporary-file-directory "tramp-" (user-login-name)))
  (message "Lazy loaded tramp :-)"))

(with-eval-after-load 'vc
  (setq vc-follow-symlinks t)
  (setq vc-make-backup-files t)
  (setq version-control t)
  (message "Lazy loaded vc :-)"))

;;;###autoload
(defun my/vc-dir (&optional arg)
  "Run `vc-dir' for the current project or directory.
With optional ARG (\\[universal-argument]), use the present
working directory, else default to the root of the current
project, as defined by `vc-root-dir'."
  (interactive "P")
  (let ((dir (if arg default-directory (vc-root-dir))))
    (vc-dir dir)))

(if (version< emacs-version "28")
    (global-set-key (kbd "C-x v d") 'my/vc-dir)
  (global-set-key (kbd "C-x v d") 'vc-dir-root))

(with-eval-after-load 'whitespace
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
  (message "Lazy loaded whitespace :-)"))

(add-hook 'before-save-hook 'whitespace-cleanup)

(let ((lazygit-directory (expand-file-name "~/src/gitlab/tspub/lisp/lazygit")))
  (when (file-directory-p lazygit-directory)
    (add-to-list 'load-path lazygit-directory)

    (with-eval-after-load 'lazygitlab
      (setq lazygit-directory (expand-file-name "~/src/gitlab"))
      (message "Lazy loaded lazygitlab :-)"))

    (autoload 'lazygit-status-all "lazygit" nil t)
    (global-set-key (kbd "C-c g s") 'lazygit-status-all)
    (autoload 'lazygit-pull-all "lazygit" nil t)
    (global-set-key (kbd "C-c g p") 'lazygit-pull-all)

    (autoload 'lazygitlab-clone-or-pull-all "lazygitlab" nil t)
    (global-set-key (kbd "C-c g l a") 'lazygitlab-clone-or-pull-all)
    (autoload 'lazygitlab-clone-or-pull-group "lazygitlab" nil t)
    (global-set-key (kbd "C-c g l g") 'lazygitlab-clone-or-pull-group)
    (autoload 'lazygitlab-clone-or-pull-project "lazygitlab" nil t)
    (global-set-key (kbd "C-c g l p") 'lazygitlab-clone-or-pull-project)

    (with-eval-after-load 'lazygithub
      (setq lazygit-directory (expand-file-name "~/src/github"))
      (message "Lazy loaded lazygithub :-)"))

    (autoload 'lazygithub-clone-or-pull-all "lazygithub" nil t)
    (global-set-key (kbd "C-c g h a") 'lazygithub-clone-or-pull-all)
    (autoload 'lazygithub-clone-or-pull-repo "lazygithub" nil t)
    (global-set-key (kbd "C-c g h r") 'lazygithub-clone-or-pull-repo)))

(let ((dired-peep-directory (expand-file-name "~/src/gitlab/tspub/lisp/dired-peep")))
  (when (file-directory-p dired-peep-directory)
    (add-to-list 'load-path dired-peep-directory)
    (with-eval-after-load 'dired
      (autoload 'dired-peep-mode "dired-peep")
      (define-key dired-mode-map "r" 'dired-peep-mode)
      (define-key dired-mode-map (kbd "C-o") 'dired-peep-temporarily)
      (define-key dired-mode-map (kbd "M-o") 'dired-peep))))

(provide 'init)
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; init.el ends here
