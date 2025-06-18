(use-package
 cargo
 :after rust-ts-mode
 :hook (cargo-minor-mode-hook . rust-ts-mode))

(use-package
 rust-ts-mode
 :hook
 ((rust-ts-mode-hook . (lambda () (setq indent-tabs-mode nil))))
 :custom (rust-format-on-save t))
