(use-package
 go-ts-mode
 :mode (("\\.go\\'" . go-ts-mode))
 :custom (godoc-reuse-buffer t))

(use-package gorepl-mode :hook (go-ts-mode . gorepl-mode))

(use-package gotest)

(use-package
 flycheck-golangci-lint
 :ensure t
 :hook (go-ts-mode . flycheck-golangci-lint-setup))

(use-package go-tag)
