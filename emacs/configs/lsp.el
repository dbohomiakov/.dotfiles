(use-package lsp-mode
  ;; :after (pyvenv-mode)
  :bind (:map lsp-mode-map
        ("TAB" . completion-at-point))
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  :hook ((rust-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         ;; (clojure-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))


(defun db/configure-lsp ()
  (setq lsp-disabled-clients '(mspyls pylsp pyls))
  (setq lsp-completion-provider :capf)
  (setq lsp-idle-delay 0.500)
  (setq lsp-lens-enable t)
  (setq lsp-log-io nil) ;; if set to true can cause a performance hit
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-file-watch-threshold 20000)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-eldoc-enable-hover t)
  (setq lsp-completion-show-detail t)
  (setq lsp-completion-show-kind t)
  ;; (setq lsp-diagnostics-provider nil)
  ;; (setq lsp-diagnostics-disabled-modes '(python-mode))
  (setq lsp-use-plists t)
  ;; (setq lsp-diagnostics-provider nil)
  ;; (setq lsp-diagnostics-disabled-modes '(python-mode))
  (setq lsp-headerline-breadcrumb-enable nil))

(db/configure-lsp)

;; TODO: move to .dir-locals
;; add dirs excluded from watching
(with-eval-after-load 'lsp-mode
  (mapcar
   (lambda (x) (add-to-list 'lsp-file-watch-ignored-directories x))
   '("[/\\\\]\\static\\'"
     "[/\\\\]\\__pycache__\\'"
     "[/\\\\]\\.mypy_cache\\'")))

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  :init
  (lsp-treemacs-sync-mode 1))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred)))
  :custom
  (lsp-pyright-auto-import-completions t)
  (lsp-pyright-venv-path "/home/dbohomiakov/.virtualenvs/"))

;; (mapcar 'lsp-workspace-folders-remove (lsp-session-folders (lsp-session)))

(use-package consult-lsp
  :straight (:host github :repo "gagbo/consult-lsp"))

(require 'eldoc-box)
