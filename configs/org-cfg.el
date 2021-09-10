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

(use-package
  org-roam
  :ensure t
  :init (setq org-roam-v2-ack t)
  (setq org-roam-completion-everywhere t)
  (setq org-roam-dailies-directory "/daily")
  :custom (org-roam-directory "~/RoamNotes")
  :config (org-roam-setup))

(use-package
  org-ref)
