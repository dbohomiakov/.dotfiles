;;;  -*- lexical-binding: t -*-
(defun set-display-fill-column-indicator (num)
  (progn
    (setq-default display-fill-column-indicator-column num)
    (add-hook 'python-mode-hook 'display-fill-column-indicator-mode)))

;; (defun setup-flake8-flycheck ()
;;   (setq flycheck-checkers '(python-flake8))
;;   (flycheck-mode))

(defun fix-python-indent ()
  (setq python-indent-offset 4)
  (setq tab-width 4)
  (subword-mode 1))

(use-package python-mode
  ;; :bind ("TAB" . company-indent-or-complete-common)
  :hook ((python-mode . fix-python-indent)
         ;; (python-mode . setup-flake8-flycheck)
         (python-mode . (lambda ()
                               (set-display-fill-column-indicator 79)))
         (python-mode . hs-minor-mode)
         (python-mode . yas-minor-mode)
         (python-mode . abbrev-mode)))

;; TODO: move selection of python interpreter to .dir-locals
(defun db/setup-python-shell-interpreter ()
  (interactive)
  (let ((python-path (concat pyvenv-virtual-env "bin/python")))
    (when (executable-find python-path)
        (setq python-shell-interpreter python-path))))

;; TODO: move selection of python interpreter to .dir-locals
(defun db/setup-python-shell-interpreter ()
  (interactive)
  (let ((python-path (concat pyvenv-virtual-env "bin/python")))
    (when (executable-find python-path)
        (setq python-shell-interpreter python-path))))

;; TODO: move selection of python interpreter to .dir-locals
(defun db/setup-python-shell-interpreter ()
  (interactive)
  (let ((python-path (concat pyvenv-virtual-env "bin/python")))
    (when (executable-find python-path)
        (setq python-shell-interpreter python-path))))

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
  :config
  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list 'db/setup-python-shell-interpreter))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python")))))

;; Initialize .dir-locals variables
(setq db/pname nil)
(setq db/plang nil)
(setq db/venv-path nil)
(setq db/auto-isort nil)
(setq db/remove-unused-imports nil)
(setq db/auto-blacken nil)
(setq db/exclude-from-blacken '())
(setq db/lsp-enable? nil)
(setq db/test-command nil)

(defun db/setup-lsp-workspaces ()
  (let*
    ((project-path (projectile-project-root))
      (workspace-folders-to-remove
        (lsp-session-folders (lsp-session)))
      ;; setup custom venv-path or build from envs-path and project name
      (venv-path (or
                  db/venv-path
                  (concat (pyvenv-workon-home) "/" db/pname)))
      (workspace-folders-to-add `(,project-path ,venv-path)))
    (progn
      (print (format "%s setup!" db/pname))
      (mapcar
        'lsp-workspace-folders-remove
        workspace-folders-to-remove)
      (mapcar 'lsp-workspace-folders-add workspace-folders-to-add))))

(defun db/setup-lsp ()
  (when db/lsp-enable?
    (db/setup-lsp-workspaces)))

(defun db/enable-python-venv ()
  (let ((pname db/pname))
    (progn
      (pyvenv-workon pname)
      (unless (file-directory-p pyvenv-virtual-env)
        (progn
          (pyvenv-deactivate)
          (call-interactively #'pyvenv-workon))))))

(defun db/setup-test-settings ()
  (when db/test-command
    (setq python-pytest-executable db/test-command)))

(defun db/configure-python-project ()
  (interactive)
  (progn
    (db/enable-python-venv)
    (db/setup-test-settings)
    (db/setup-lsp)))

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

;; Tests
(defun db/-project-rel-file-path ()
  (interactive)
  (string-remove-prefix (projectile-project-root) (buffer-file-name)))

(defun db/compose-django-tests-path ()
  (interactive)
  (replace-regexp-in-string "/" "." (string-remove-suffix ".py" (db/-project-rel-file-path))))

(defun db/-module-test-path ()
  (interactive)
  (which-function))

(defun db/compose-pytest-test-name ()
  (interactive)
  (kill-new (concat (db/-project-rel-file-path) "::" (replace-regexp-in-string "\\." "::" (db/-module-test-path)))))

(defun db/compose-django-test-name ()
  (interactive)
  (kill-new (concat (compose-django-file-test-path) "." (db/-module-test-path))))

(defun db/compose-pytest ()
  (interactive)
  (kill-new (concat python-pytest-executable
                    " "
                    (db/compose-pytest-test-name))))

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
