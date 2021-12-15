(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

(use-package rust-mode
  :config
  (setq rust-format-on-save t))

(use-package cargo)

(use-package racer
  :bind (:map rust-mode-map
              ("TAB" . company-indent-or-complete-common))
  :hook
  (rust-mode-hook . racer-mode)
  (racer-mode-hook . eldoc-mode)
  (racer-mode-hook . cargo-minor-mode)
  (racer-mode-hook . company-mode))

;; (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

;; (add-hook 'rust-mode-hook 'cargo-minor-mode)
