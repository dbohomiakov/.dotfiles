(use-package cargo)

(use-package rust-mode
  :bind ("TAB" . company-indent-or-complete-common)
  :hook ((rust-mode-hook . (lambda () (setq indent-tabs-mode nil)))
         (rust-mode-hook . cargo-minor-mode))
  :custom
  (rust-format-on-save t))
