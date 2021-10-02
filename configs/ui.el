;; Folder for searching custom themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/custom-themes/")

(use-package all-the-icons
  :diminish)

(use-package icons-in-terminal
  :straight (:host github :repo "seagle0128/icons-in-terminal.el"))

(icons-in-terminal-icon-for-mode 'dired-mode)

(use-package doom-themes
  :config
  (setq doom-gruvbox-light-variant "soft")
  (setq doom-gruvbox-dark-variant "hard")
  (load-theme 'my-doom-solarized-dark t))

;; Vertical window divider
(use-package frame
  :straight (:type built-in)
  :custom
  (window-divider-default-right-width 12)
  (window-divider-default-bottom-width 1)
  (window-divider-default-places 'right-only)
  (window-divider-mode t))
;; Make sure new frames use window-divider
(add-hook 'before-make-frame-hook 'window-divider-mode)

;; Dim inactive windows
(use-package dimmer
  :straight (:host github :repo "gonewest818/dimmer.el")
  :hook (after-init . dimmer-mode)
  :config
  (setq dimmer-fraction 0.5)
  (setq dimmer-adjustment-mode :foreground)
  (setq dimmer-use-colorspace :rgb)
  (setq dimmer-watch-frame-focus-events nil)
  (dimmer-configure-which-key)
  (dimmer-configure-magit)
  (dimmer-configure-posframe))

;; Fancy modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Emoji
(use-package emojify
  :hook (erc-mode . emojify-mode)
  :commands emojify-mode)

;; Fancy show background
(use-package snow)

(defun db/set-font-faces ()
  ;; (set-face-attribute 'default nil :font "Fira Code Retina" :height 120)
  (set-face-attribute 'default nil :font "JetBrains Mono Nerd Font Mono" :height 115 :weight 'light)
  ;; Set the fixed pitch face
  ;; (set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 100)
  (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono Nerd Font Mono" :height 100 :weight 'light)
  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :font "JetBrains Mono Nerd Font Mono" :height 50 :weight 'light))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook
              (lambda ()
                (setq doom-modeline-icon t)
                (db/set-font-faces)))
  (progn
   (db/set-font-faces)))

(setq echo-keystrokes 0.1)
(setq-default line-spacing 0.1)
