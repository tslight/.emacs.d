;; This must be true otherwise use-package won't load!
(setq package-enable-at-startup t)
;; Allow loading from the package cache.
(setq package-quickstart t)
;; Don't write (package-initialize) to my init file!
(setq package--init-file-ensured t)
;; Setup up archives
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
