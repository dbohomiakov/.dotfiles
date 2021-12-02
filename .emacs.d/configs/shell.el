(use-package vterm)

(use-package vterm-toggle
  :ensure t
  :custom
  (vterm-toggle-scope 'project)
  (vterm-toggle-hide-method 'reset-window-configration)
  :hook
  (vterm-toggle-show . evil-insert-state))

(setq vterm-toggle-fullscreen-p t)
(add-to-list 'display-buffer-alist
      '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
         (display-buffer-reuse-window display-buffer-same-window)))
(global-set-key [f2] 'vterm-toggle)
(global-set-key [C-f2] 'vterm-toggle-cd)
