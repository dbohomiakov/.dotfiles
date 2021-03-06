(use-package projectile
  :config (projectile-mode)
  :custom ((projectile-completion-system 'default))
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/work/")
    (setq projectile-project-search-path '("~/work/" "~/work/bcd/" "~/work/clojure")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package consult-projectile
  :after projectile
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master"))
