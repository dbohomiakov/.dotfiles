;;;  -*- lexical-binding: t -*-
(add-hook 'python-mode-hook 'hs-minor-mode)
(add-hook 'python-mode-hook 'yas-minor-mode)

;; Fix python indent
(add-hook 'python-mode-hook
  (lambda ()
    (progn
      (setq tab-width 4)
      (subword-mode 1))))

;; optionally if you want to use debugger
;; (use-package dap-python)

;; Pyenv
(use-package pyenv-mode
  :init
  (let ((pyenv-path (expand-file-name "~/.pyenv/bin")))
    (setenv "PATH" (concat pyenv-path ":" (getenv "PATH")))
    (add-to-list 'exec-path pyenv-path))
  :config (pyenv-mode))

(use-package python-pytest
  :init
  (setq pytest-cmd-flags "--pdbcls=IPython.terminal.debugger:Pdb")
  :custom
  (python-pytest-confirm t)
  (python-pytest-pdb-track t))

(use-package py-isort
  :after python
  :hook (python-mode . pyvenv-mode))

(use-package pyimport)

(use-package blacken :delight)

(use-package pyvenv
  :ensure t
  :init (setenv "WORKON_HOME" "~/.pyenv/versions")
  :config
  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
    (list
      (lambda ()
        (setq python-shell-interpreter
          (concat pyvenv-virtual-env "bin/python")))))
  (setq pyvenv-post-deactivate-hooks
    (list (lambda () (setq python-shell-interpreter "python")))))

;; Initialize .dir-locals variables
(setq db/pname nil)
(setq db/plang nil)
(setq db/venv-path nil)
(setq db/actual-venv-path nil)
(setq db/auto-isort nil)
(setq db/remove-unused-imports nil)
(setq db/auto-blacken nil)
(setq db/lsp-enable? nil)

(defun db/add-lsp-workspaces ()
  (let*
    (
      (project-path (projectile-project-root))
      (workspace-folders-to-remove
        (lsp-session-folders (lsp-session)))
      (workspace-folders-to-add
        `
        (,project-path
          ,db/venv-path
          ,db/actual-venv-path
          "/home/dbohomiakov/.pyenv/")))
    (progn
      (print (format "%s setup!" db/pname))
      (setq lsp-pyright-venv-directory db/pname)
      (mapcar
        'lsp-workspace-folders-remove
        workspace-folders-to-remove)
      (mapcar 'lsp-workspace-folders-add workspace-folders-to-add))))

(defun db/run-lsp ()
  (interactive)
  (let ((lsp-activated? (lsp-session-folders (lsp-session))))
    (progn
      (when lsp-activated?
        (lsp-workspace-shutdown))
      (db/add-lsp-workspaces))))

(defun db/enable-lsp ()
  (when db/lsp-enable?
    (db/run-lsp)))

(defun db/enable-python-venv ()
  (let ((pname db/pname))
    (progn
      (pyvenv-workon pname)
      (unless (file-directory-p pyvenv-virtual-env)
        (progn
          (pyvenv-deactivate)
          (call-interactively #'pyvenv-workon))))))

(defun db/configure-python-project ()
  (progn
    (db/enable-python-venv)
    (db/enable-lsp)))

(defun db/do-nothing () nil)

(setq db/configure-fn-by-lang
  '(("python" . db/configure-python-project) (nil . db/do-nothing)))

(defun db/configure-project ()
  (interactive)
  (let ((configure-fn (cdr (assoc db/plang db/configure-fn-by-lang))))
    (funcall configure-fn)))

(defun db/py-isort-before-save ()
  (when db/auto-isort
    (py-isort-before-save)))

(defun db/pyimport-remove-unused ()
  (when db/remove-unused-imports
    (pyimport-remove-unused)))

(defun db/blacken-buffer ()
  (when (and db/auto-blacken)
    (blacken-buffer)))

(use-package pony-mode)

;; (use-package buftra
  ;; :straight (:host github :repo "humitos/buftra.el"))

;; (use-package py-autoflake
;;   :straight (:host github :repo "humitos/py-cmd-buffer.el")
;;   ;; :hook (python-mode . py-autoflake-enable-on-save)
;;   :config
;;   (setq py-autoflake-options '("--expand-star-imports")))

;; (add-hook 'before-save-hook 'py-autoflake-buffer nil t)
;; (add-hook 'before-save-hook 'db/pyimport-remove-unused)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'db/blacken-buffer)
(add-hook 'before-save-hook 'db/py-isort-before-save)
(add-hook 'projectile-after-switch-project-hook 'db/configure-project)

(fset 'ipdb
  (kmacro-lambda-form
    [?i
      ?m
      ?p
      ?o
      ?r
      ?t
      ?
      ?i
      ?p
      ?d
      ?b
      ?\;
      ?i
      ?p
      ?d
      ?b
      ?.
      ?s
      ?e
      ?t
      ?_
      ?t
      ?r
      ?a
      ?c
      ?e
      ?\(
      ?c
      ?o
      ?n
      ?t
      ?e
      ?x
      ?t
      ?=
      ?5
      ?0
      escape]
    0 "%d"))
