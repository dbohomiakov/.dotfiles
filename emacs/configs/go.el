(use-package go-mode
  :mode
  (("\\.go\\'" . go-mode)))

(use-package gorepl-mode
  :hook (go-mode . gorepl-mode))

(use-package gotest)

(use-package flycheck-golangci-lint
  :ensure t
  :hook (go-mode . flycheck-golangci-lint-setup))
