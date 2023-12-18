(use-package
 treemacs
 :ensure t
 :defer t
 :init
 (with-eval-after-load 'winum
   (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
 :custom
 (treemacs-workspace-switch-cleanup 'files)
 (treemacs-project-follow-cleanup t)
 :config
 (progn
   (setq treemacs-collapse-dirs
         (if treemacs-python-executable
             3
           0))

   ;;   ;; The default width and height of the icons is 22 pixels. If you are
   ;;   ;; using a Hi-DPI display, uncomment this to double the icon size.
   (treemacs-resize-icons 22)
   (treemacs-tag-follow-mode t)
   (treemacs-filewatch-mode)
   (treemacs-fringe-indicator-mode 'always)
   (treemacs-indent-guide-mode t)))

(use-package treemacs-evil :after treemacs evil :ensure t)

(use-package treemacs-projectile :after treemacs projectile :ensure t)

(use-package
 treemacs-all-the-icons
 :config (treemacs-load-theme "all-the-icons"))

(use-package treemacs-magit :after treemacs magit :ensure t)

(use-package
 treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
 :after treemacs persp-mode ;;or perspective vs. persp-mode
 :ensure t
 :config (treemacs-set-scope-type 'Perspectives))
