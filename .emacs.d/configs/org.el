(use-package
  org
  :config (setq org-agenda-files '("~/Org/Notes.org"))
  (setq org-default-notes-file "~/Org/Notes.org")
  (setq org-ellipsis " ▾"))

(setq org-agenda-include-diary t)

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
