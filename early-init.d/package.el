;;; package.el --- package configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Package Manager Configuration

;; Copyright (C) 2020 Toby Slight
;; Author: Toby Slight <toby@probook>

;;; Code:
(require 'package)
;; Initialise installed packages
(setq package-enable-at-startup t)
;; Allow loading from the package cache.
(setq package-quickstart t)
(setq package--init-file-ensured t)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; package.el ends here
