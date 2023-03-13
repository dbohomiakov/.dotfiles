(use-package eglot
  :init
  (setq eglot-stay-out-of '(company eldoc flymake)
        read-process-output-max (* 1024 1024))
  :hook
  ((python-mode . eglot-ensure)
   (rust-mode . eglot-ensure)
   (go-mode . eglot-ensure))
  :custom
  (eglot-sync-connect 1)
  (eglot-connect-timeout 10)
  (eglot-extend-to-xref t)
  (eglot-send-changes-idle-time 0.5)
  (eglot-auto-display-help-buffer nil)
  (eglot-autoshutdown t))

(use-package consult-eglot
  :custom
  (consult-eglot-ignore-column t))
