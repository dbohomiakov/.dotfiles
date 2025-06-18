(use-package
 org
 :straight (:type built-in)
 :mode ("\\.org\\'" . org-mode)
 :config
 ;; (setq org-agenda-files
 ;;       '("~/Org/PersonalAgendaNotes.org" "~/Org/WorkAgendaNotes.org"))
 (setq org-default-notes-file "~/Org/Notes.org")
 (setq org-ellipsis " ▾")
 :hook (org-mode . visual-line-mode)
 :custom
 (org-startup-with-inline-images t)
 (org-agenda-include-diary t)
 (org-confirm-babel-evaluate nil)
 (org-src-preserve-indentation t)
 (org-hide-emphasis-markers t)
 (org-todo-keywords
  '((sequence
     "TODO(t)"
     "WAIT(w@/!)"
     "PROGRESS(p!)"
     "|"
     "DONE(d!)"
     "CANCELED(c@)"))))

(use-package
 org-bullets
 :after org
 :hook (org-mode . org-bullets-mode)
 :custom (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package
 org-modern-indent
 :straight
 (org-modern-indent
  :type git
  :host github
  :repo "jdtsmith/org-modern-indent")
 :config (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package
 org-modern
 :custom
 (org-modern-hide-stars nil) ; adds extra indentation
 (org-modern-table nil)
 (org-modern-list
  '( ;; (?- . "-")
    (?* . "•")
    (?+ . "‣")))
 :hook
 (org-mode . org-modern-mode)
 (org-agenda-finalize . org-modern-agenda))

(use-package
 org-roam
 :after org
 :init (setq org-roam-v2-ack t)
 :custom
 (org-roam-directory "~/RoamNotes") ;; TODO: store everything in one directory
 (org-roam-completion-everywhere t)
 (org-roam-dailies-directory "~/RoamNotes/daily")
 :config (org-roam-setup))

(use-package org-ref :after org :defer)

(use-package ob-ipython :after org :defer)

(use-package
 evil-org
 :ensure t
 :after org
 :hook (org-mode . (lambda () evil-org-mode))
 :custom (calendar-week-start-day 1)
 :config
 (require 'evil-org-agenda)
 (evil-org-agenda-set-keys))

(use-package
 org-present
 :ensure t
 :straight
 (org-present :type git :host github :repo "rlister/org-present"))

(use-package
 visual-fill-column
 :custom
 (visual-fill-column-width 110)
 (visual-fill-column-center-text t))

;; (setq org-plantuml-jar-path
;;       (expand-file-name
;;        "/home/dbohomiakov/.config/emacs/plantuml.jar"))
;; (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
;; (org-babel-do-load-languages
;;  'org-babel-load-languages '((plantuml . t)))

(use-package
 plantuml-mode
 :config
 (setq org-plantuml-jar-path
       (expand-file-name
        "/home/dbohomiakov/.config/emacs/plantuml.jar"))
 (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
 (org-babel-do-load-languages
  'org-babel-load-languages '((plantuml . t))))

(use-package
 org-tree-slide
 :bind (("<f8>" . 'org-tree-slide-mode))
 ;; ("M-l" . 'org-tree-slide-move-next-tree)
 ;; ("M-h" . 'org-tree-slide-move-prev-tree)

 :custom (org-image-actual-width nil))

(use-package org-transclusion)

(use-package org-roam-ui)
