(use-package
  lsp-mode
  :after (pyenv-mode)
  :init (setq lsp-disabled-clients '(mspyls pylsp pyls))
  (setq lsp-completion-provider
        :capf)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-idle-delay 0.500)
  (setq lsp-lens-enable t)
  (setq lsp-log-io nil) ;; if set to true can cause a performance hit
  (setq lsp-enable-on-type-formatting t)
  (setq lsp-file-watch-threshold 2000)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-eldoc-enable-hover t)
  (setq lsp-completion-show-detail t)
  (setq lsp-completion-show-kind t)
  (setq lsp-diagnostics-provider nil)
  (setq lsp-diagnostics-disabled-modes '(python-mode))
  :hook ((rust-mode . lsp-deferred)
         ;; (clojure-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

;; TODO: move to .dir-locals
;; add dirs excluded from watching
(with-eval-after-load 'lsp-mode
  (mapcar
   (lambda (x) (add-to-list 'lsp-file-watch-ignored-directories x))
   '("[/\\\\]\\static\\'"
     "[/\\\\]\\__pycache__\\'"
     "[/\\\\]\\.mypy_cache\\'")))

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
  ;; (lsp-pyright-venv-path "/home/dbohomiakov/.pyenv/versions")
  )

(mapcar 'lsp-workspace-folders-remove (lsp-session-folders (lsp-session)))

(use-package
  yasnippet
  :commands yas-minor-mode
  :hook ((python-mode . yas-minor-mode)))

(use-package consult-lsp)

;; (use-package eglot)
;; (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
