;;; minibuffer.el --- minibuffer/completion configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Extend minibuffer and better default complete engine/settings

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <tslight@pm.me>

;;; Code:
(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(setq savehist-save-minibuffer-history 1)

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

(add-hook 'minibuffer-exit-hook (lambda () (setq gc-cons-threshold 800000)))
(add-hook 'minibuffer-setup-hook (lambda () (setq gc-cons-threshold most-positive-fixnum)))
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; minibuffer.el ends here
