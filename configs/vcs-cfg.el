;; Git
(use-package magit
  :config
  (setq magit-view-git-manual-method 'man))

;; (use-package magit-todos)

;; (use-package magit-delta)

;; (add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1)))

;; diff-hl
;; (use-package diff-hl)

;; (global-diff-hl-mode +1)

;; (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
;; (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;;git gutter
(use-package git-gutter)

;; (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
