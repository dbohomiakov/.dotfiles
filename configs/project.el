(use-package
  projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'default))
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/work/")
    (setq projectile-project-search-path '("~/work/" "~/work/bcd/" "~/work/clojure")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package treemacs)

(use-package consult-projectile
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master"))

(defun compose-django-file-test-path ()
  (interactive)
  (replace-regexp-in-string "/" "."
                            (string-remove-prefix (projectile-project-root)
                                                  (string-remove-suffix ".py" (buffer-file-name)))))

(defun compose-django-test-name ()
  (interactive)
  (kill-new (concat (compose-django-file-test-path) "." (which-function))))

(defun run-django-test-at-point ()
  (let ((buf-path ))))
