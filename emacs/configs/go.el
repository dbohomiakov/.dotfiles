(use-package
 go-mode
 :mode (("\\.go\\'" . go-mode))
 :custom (godoc-reuse-buffer t))

(use-package gorepl-mode :hook (go-mode . gorepl-mode))

(use-package gotest)

(use-package
 flycheck-golangci-lint
 :ensure t
 :hook (go-mode . flycheck-golangci-lint-setup))

(use-package go-tag)

(use-package k8s-mode :ensure t :hook (k8s-mode . yas-minor-mode))

(use-package
 kubernetes
 :ensure t
 :commands (kubernetes-overview)
 :config
 (setq
  kubernetes-poll-frequency 3600
  kubernetes-redraw-frequency 3600))
