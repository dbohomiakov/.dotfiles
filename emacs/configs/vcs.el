;; Git
(use-package magit
  :config
  (setq magit-view-git-manual-method 'man))

(use-package magit-todos
  :after magit
  :init
  (magit-todos-mode))

(use-package magithub
  :after magit
  :config
  (magithub-feature-autoinject t)
  (setq magithub-clone-default-directory "~/github"))

(use-package forge
  :after magit
  :init
  (setq auth-sources '("~/.authinfo")))

;; Show changed lines in linenum modeline
(use-package diff-hl
  :config
  (global-diff-hl-mode))

(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(use-package ibuffer-vc
  :init (setq ibuffer-vc t))

(use-package git-link
  :custom
  (git-link-open-in-browser t)
  (git-link-use-single-line-number t)
  (git-link-use-commit t))

(use-package git-modes)
