  ;; Projectile
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/work/")
    (setq projectile-project-search-path '("~/work/" "~/work/bcd/" "~/work/clojure")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config
  (setq counsel-projectile-preview-buffers nil))

(use-package treemacs)

(defun db/counsel-projectile-rg (&optional arg)
  (interactive)
  (counsel-projectile-rg arg))
