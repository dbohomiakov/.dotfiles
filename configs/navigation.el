(use-package avy)

(use-package ace-window
  :init
  (setq aw-keys '(?h ?j ?k ?l)))

;; Move blocks of code horizontaly
(use-package drag-stuff
  :init
  (drag-stuff-mode t)
  (drag-stuff-define-keys))

(use-package aggressive-indent)

(use-package evil-mc
  :init
  (global-evil-mc-mode  1))

;; Multiple cursors
(use-package
  multiple-cursors)

(use-package transpose-frame)

(use-package expand-region)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq use-dialog-box nil) ;; Disable dialog boxes since they weren't working in Mac OSX
