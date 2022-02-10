;; Git
(use-package magit
  :config
  (setq magit-view-git-manual-method 'man))

(use-package forge
  :after magit
  :init
  (setq auth-sources '("~/.authinfo")))

;; Show changed lines in linenum modeline
(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

(use-package ibuffer-vc
  :init (setq ibuffer-vc t))

(use-package browse-at-remote
  :straight (:host github :repo "rmuslimov/browse-at-remote"))

(use-package git-timemachine)
