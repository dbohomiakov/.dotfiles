(use-package magit
  :config
  (setq magit-git-command-history t)
  :custom
  (magit-update-other-window-delay 1)
  (magit-view-git-manual-method 'man))

(use-package magit-todos
  :after magit
  :init
  (magit-todos-mode))

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

(use-package git-link
  :custom
  (git-link-open-in-browser t)
  (git-link-use-single-line-number t)
  (git-link-use-commit t))

(use-package git-modes)
