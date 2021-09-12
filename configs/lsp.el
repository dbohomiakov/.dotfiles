(use-package
  lsp-mode
  :after (pyenv-mode)
  :init (setq lsp-disabled-clients '(mspyls))
  (setq lsp-completion-provider
        :capf)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-idle-delay 0.300)
  (setq lsp-lens-enable t)
  (setq lsp-log-io nil) ;; if set to true can cause a performance hit
  (setq lsp-enable-on-type-formatting t)
  (setq lsp-file-watch-threshold 85000)
  (setq lsp-file-watch-ignored '("static/**"))
  :hook ((rust-mode . lsp-deferred)
         ;; (clojure-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

(use-package
  lsp-ui
  :commands lsp-ui-mode
  :init (setq lsp-ui-doc-enable nil))

(use-package
  lsp-treemacs
  :commands lsp-treemacs-errors-list)

(use-package
  lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred)))
  :custom (lsp-pyright-auto-import-completions t)
  (lsp-pyright-venv-path "/home/dbohomiakov/.pyenv/versions"))

(mapcar 'lsp-workspace-folders-remove (lsp-session-folders (lsp-session)))

(use-package
  yasnippet
  :commands yas-minor-mode
  :hook ((python-mode . yas-minor-mode)))

(use-package consult-lsp)
