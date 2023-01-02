(electric-indent-mode +1)

(use-package avy)

(use-package ace-window
  :custom
  (aw-scope 'frame)
  (aw-keys '(?h ?j ?k ?l))
  (aw-minibuffer-flag t)
  :config
  (ace-window-display-mode 1))

;; Move blocks of code horizontaly
(use-package drag-stuff
  :init
  (drag-stuff-global-mode 1)
  :config
  (drag-stuff-define-keys))

(use-package evil-mc
  :after evil
  :init
  (global-evil-mc-mode 1))

(use-package transpose-frame)

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq use-dialog-box nil) ;; Disable dialog boxes since they weren't working in Mac OSX

(use-package evil-lion
  :after evil
  :ensure t
  :config
  (evil-lion-mode))
