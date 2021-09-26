;; Git
(use-package magit
  :config
  (setq magit-view-git-manual-method 'man))

(use-package forge
  :after magit)

;; (use-package magit-todos)

;; (use-package magit-delta)

;; (add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1)))

;; diff-hl
;; (use-package diff-hl)

;; (global-diff-hl-mode +1)

;; (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
;; (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; Show changed lines in linenum modeline
(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

(use-package
  ibuffer-vc
  :init (setq ibuffer-vc t))

;; (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

(use-package browse-at-remote
  :straight (:host github :repo "rmuslimov/browse-at-remote"))
