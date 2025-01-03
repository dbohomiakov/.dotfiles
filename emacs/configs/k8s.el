(use-package k8s-mode :ensure t :hook (k8s-mode . yas-minor-mode))

(use-package
 kubernetes
 :ensure t
 :commands (kubernetes-overview)
 :config
 (setq
  kubernetes-poll-frequency 3600
  kubernetes-redraw-frequency 3600))

(use-package kele :straight t :config (kele-mode 1))

(use-package kubel :after (vterm) :config (kubel-vterm-setup))
