(use-package eglot
  :config
  (setq eglot-stay-out-of '(eldoc flymake))
  :custom
  (eglot-sync-connect 1)
  (eglot-connect-timeout 10)
  (eglot-extend-to-xref t)
  (eglot-send-changes-idle-time 0.5)
  (eglot-auto-display-help-buffer nil)
  (eglot-autoshutdown t))

(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'eglot-ensure)
