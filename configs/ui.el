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

;; Fancy modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Fancy show background
(use-package snow)

(defun db/set-font-faces ()
  (set-face-attribute 'default nil :font "JetBrains Mono Nerd Font Mono" :height 115 :weight 'light)
  ;; Set the fixed pitch face
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
