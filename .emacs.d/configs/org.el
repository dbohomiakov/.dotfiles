(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config (setq org-agenda-files '("~/Org/Notes.org"))
  (setq org-default-notes-file "~/Org/Notes.org")
  (setq org-ellipsis " ▾")
  ; (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  :hook (org-mode . visual-line-mode)
  :custom
  (org-agenda-include-diary t)
  (org-confirm-babel-evaluate nil)
  (org-src-preserve-indentation t)
  (org-hide-emphasis-markers t))

(use-package
  org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-roam
  :after org
  :init (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/RoamNotes")
  (org-roam-completion-everywhere t)
  (org-roam-dailies-directory "/daily")
  :config (org-roam-setup))

(use-package org-ref
  :after org
  :defer)

(use-package ob-ipython
  :after org
  :defer)

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-present
  :ensure t
  :straight (org-present :type git :host github :repo "rlister/org-present"))
