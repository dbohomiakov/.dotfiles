(use-package go-mode
  :mode
  (("\\.go\\'" . go-mode)))

(use-package gorepl-mode
  :hook (go-mode . gorepl-mode))
