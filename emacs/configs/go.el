(use-package go-mode
  :defer t)

(use-package gorepl-mode
  :hook (go-mode . gorepl-mode))
