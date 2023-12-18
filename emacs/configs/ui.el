;; Folder for searching custom themes
(add-to-list
 'custom-theme-load-path (concat db/emacs-dir "/custom-themes/"))

;; Show battery percentage, current time
(display-battery-mode 1)

(use-package
 time
 :init (display-time-mode 1)
 :custom
 (display-time-interval 60)
 (display-time-24hr-format t)
 ;; remove average load
 (display-time-load-average t)
 (display-time-load-average-threshold 1000))

(use-package all-the-icons :diminish)

(use-package
 all-the-icons-dired
 :straight (:host github :repo "wyuenho/all-the-icons-dired")
 :hook (dired-mode . all-the-icons-dired-mode)
 :custom (all-the-icons-dired-monochrome nil))

(use-package
 doom-themes
 :config (setq doom-gruvbox-light-variant "soft"))

;; `gruvbox-material' contrast and palette options
(setq
 doom-gruvbox-material-background "hard" ; or hard (defaults to soft)
 doom-gruvbox-material-palette "material") ; or original (defaults to material)
;; `gruvbox-material-light' contrast and palette options
(setq
 doom-gruvbox-material-light-background
 "hard" ; or hard (defaults to soft)
 doom-gruvbox-material-light-palette "material") ; or original (defaults to material)

;; Use different themes for terminal/GUI
(if (display-graphic-p)
    (progn
      (defvar db/dark-theme 'my-doom-solarized-dark)
      (defvar db/light-theme 'doom-gruvbox-light))
  (progn
    (defvar db/dark-theme 'my-doom-solarized-dark)
    (defvar db/light-theme 'doom-gruvbox-light)))

;; Fix theme applying for emacsclient in terminal mode
(defun db/load-theme (frame)
  (select-frame frame)
  (load-theme db/dark-theme t))

;; Setup default theme
(if (daemonp)
    (add-hook 'after-make-frame-functions #'db/load-theme)
  (load-theme db/dark-theme t))

(defun db/toggle-theme ()
  (interactive)
  (let ((enabled-theme (car custom-enabled-themes)))
    (disable-theme enabled-theme)
    (if (equal enabled-theme db/dark-theme)
        (load-theme db/light-theme t)
      (load-theme db/dark-theme t))))

;; Vertical window divider
(use-package
 frame
 :straight (:type built-in)
 ;; Make sure new frames use window-divider
 :hook (before-make-frame-hook . window-divider-mode)
 :custom
 (window-divider-default-right-width 12)
 (window-divider-default-bottom-width 1)
 (window-divider-default-places 'right-only)
 (window-divider-mode t))

;; Fancy modeline
(use-package
 doom-modeline
 :ensure t
 :hook (after-init . doom-modeline-mode)
 :custom
 (doom-modeline-icon (display-graphic-p))
 (doom-modeline-hud nil)
 (doom-modeline-buffer-encoding nil)
 (doom-modeline-vcs-max-length 40)
 (doom-modeline-workspace-name nil)
 (doom-modeline-gnus nil)
 (doom-modeline-env-enable-python nil)
 (doom-modeline-major-mode-color-icon t)
 (doom-modeline-enable-word-count nil))

;; Hide modeline
(use-package hide-mode-line)

;; Fancy backgrounds
(use-package snow)
(use-package fireplace)

(defvar db/font-family "JetBrainsMono NL")
(defvar db/font-weight 'normal)

(defun db/set-font-faces ()
  (set-face-attribute 'default nil
                      :font db/font-family
                      :height 120
                      :weight db/font-weight)
  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil
                      :font db/font-family
                      :height 90
                      :weight 'light)
  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil
                      :font db/font-family
                      :height 120
                      :weight 'light))

(if (daemonp)
    (add-hook
     'server-after-make-frame-hook
     (lambda ()
       (setq doom-modeline-icon t)
       (db/set-font-faces)))
  (progn
    (db/set-font-faces)))

(setq echo-keystrokes 0.1)
(setq-default line-spacing 0.03)

;; Flash the current line
(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command
         '(scroll-up-command
           scroll-down-command recenter-top-bottom other-window))
  (advice-add command :after #'pulse-line))

(use-package solaire-mode :init (solaire-global-mode +1))

(use-package
 dashboard
 :demand t
 :ensure t
 :config (dashboard-setup-startup-hook)
 :custom
 (dashboard-items
  '((projects . 5) (agenda . 5) (bookmarks . 5) (registers . 5)))
 (dashboard-icon-type 'all-the-icons)
 (dashboard-set-heading-icons t)
 (dashboard-set-file-icons t)
 (dashboard-modify-heading-icons
  '((recents . "file-text") (bookmarks . "book")))
 (dashboard-set-navigator t)
 (dashboard-set-init-info t)
 (dashboard-week-agenda t))

(setq initial-buffer-choice
      (lambda () (get-buffer-create "*dashboard*")))

(use-package drepl)

(use-package popper)
