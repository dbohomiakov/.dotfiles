(use-package eglot
  :init
  (setq eglot-stay-out-of '(company eldoc flymake)
        ;; eglot-workspace-configuration
        ;; '(:pyright (:useLibraryCodeForTypes t :openFilesOnly :json-false)
        ;;            :r (:lsp (:diagnostics :json-false))
        ;;            )
        read-process-output-max (* 1024 1024))
  :custom
  (eglot-sync-connect 1)
  (eglot-connect-timeout 10)
  (eglot-extend-to-xref t)
  (eglot-send-changes-idle-time 0.5)
  (eglot-auto-display-help-buffer nil)
  (eglot-autoshutdown t)
  )

(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'eglot-ensure)

(use-package consult-eglot
  :custom
  (consult-eglot-ignore-column t)
  (consult-eglot-narrow
   '(;; Lowercase classes
     (?c . "Class")
     (?f . "Function"))))
