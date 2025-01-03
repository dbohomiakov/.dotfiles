(use-package
 cargo
 :after rust-mode
 :hook (cargo-minor-mode-hook . rust-mode))

(use-package
 rust-mode
 :hook
 ((rust-mode-hook . (lambda () (setq indent-tabs-mode nil))))
 :custom (rust-format-on-save t))

(use-package
 racer
 :hook
 (rust-mode-hook . racer-mode)
 (racer-mode-hook . eldoc-mode)
 (racer-mode-hook . cargo-minor-mode))
