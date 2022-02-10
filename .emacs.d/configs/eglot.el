(use-package eglot
  :after (pyvenv-mode)
  :config
  (setq eglot-stay-out-of '(eldoc flymake)))

(add-hook 'python-mode-hook 'eglot-ensure)
