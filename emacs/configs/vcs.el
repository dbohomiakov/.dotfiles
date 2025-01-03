(use-package
 magit
 :config (setq magit-git-command-history t)
 :custom
 (magit-update-other-window-delay 1)
 (magit-branch-read-upstream-first 'fallback)
 (magit-published-branches '("origin/master" "origin/main"))
 (magit-view-git-manual-method 'man))

(use-package magit-todos :after magit :init (magit-todos-mode))

(use-package
 forge
 :after magit
 :init (setq auth-sources '("~/.authinfo")))

;; Show changed lines in linenum modeline
(use-package diff-hl :config (global-diff-hl-mode))

(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(use-package
 git-link
 :custom
 (git-link-open-in-browser t)
 (git-link-use-single-line-number t)
 (git-link-use-commit t))

(use-package git-modes)

(use-package
 blamer
 :bind (("s-i" . blamer-show-commit-info))
 :defer 20
 :custom (blamer-idle-time 0.3) (blamer-min-offset 70)
 :custom-face
 (blamer-face
  ((t :foreground "#7a88cf" :background nil :height 140 :italic t))))

(use-package
 consult-gh
 :straight
 (consult-gh
  :type git
  :host github
  :repo "armindarvish/consult-gh"
  :after consult))

(use-package hl-todo :init (global-hl-todo-mode))
