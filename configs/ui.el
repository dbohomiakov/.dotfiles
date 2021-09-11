;; Folder for searching custom themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/custom-themes/")

(use-package all-the-icons
  :diminish)

(use-package doom-themes
  :config
  (setq doom-gruvbox-light-variant "soft")
  (setq doom-gruvbox-dark-variant "hard")
  (load-theme 'my-doom-solarized-dark t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Emoji
(use-package emojify
  :hook (erc-mode . emojify-mode)
  :commands emojify-mode)

(defun db/set-font-faces ()
  (set-face-attribute 'default nil :font "Fira Code Retina" :height 100)
  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 100)
  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :font "Fira Code Retina" :height 50 :weight 'regular))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook
              (lambda ()
                (setq doom-modeline-icon t)
                ;; (load-theme 'nord t)
                (db/set-font-faces)))
  (progn
   ;; (load-theme 'nord t)
   (db/set-font-faces)))

(setq echo-keystrokes 0.1)
(setq-default line-spacing 0.3)
