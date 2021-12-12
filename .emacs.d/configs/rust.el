(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

(use-package rust-mode)
(setq rust-format-on-save t)

(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

(use-package cargo)
(add-hook 'rust-mode-hook 'cargo-minor-mode)
