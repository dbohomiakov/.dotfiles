;;;  -*- lexical-binding: t -*-

;; Use python-ts-mode instead of python-mode
(when (fboundp 'python-ts-mode)
  (add-to-list
   'major-mode-remap-alist '(python-mode . python-ts-mode)))

(defun set-display-fill-column-indicator (num)
  (progn
    (setq-default display-fill-column-indicator-column num)))

(add-hook 'python-ts-mode-hook 'display-fill-column-indicator-mode)

(defun fix-python-indent ()
  (setq python-indent-offset 4)
  (setq tab-width 4)
  (subword-mode 1))

(defun db/setup-python-shell-interpreter ()
  (interactive)
  (let ((python-path (concat pyvenv-virtual-env "bin/python")))
    (when (executable-find python-path)
      (setq python-shell-interpreter python-path))))

(use-package
 pyvenv
 :after python-ts-mode
 :config
 ;; Set correct Python interpreter
 (setq pyvenv-post-activate-hooks
       (list 'db/setup-python-shell-interpreter))
 (setq pyvenv-post-deactivate-hooks
       (list (lambda () (setq python-shell-interpreter "python")))))

(defun db/enable-python-venv ()
  (let ((pname db/pname))
    (progn
      (pyvenv-workon pname)
      (unless (file-directory-p pyvenv-virtual-env)
        (progn
          (pyvenv-deactivate)
          (call-interactively #'pyvenv-workon))))))

(use-package
 python-ts-mode
 :straight (:type built-in)
 :hook
 ((python-ts-mode . fix-python-indent)
  (python-ts-mode
   . (lambda () (set-display-fill-column-indicator 88)))
  (python-ts-mode . yas-minor-mode) (python-ts-mode . abbrev-mode)))

(use-package
 python-pytest
 :init
 (setq pytest-cmd-flags "--pdbcls=IPython.terminal.debugger:Pdb")
 :custom
 (python-pytest-confirm t)
 (python-pytest-pdb-track t))

(use-package
 flymake-ruff
 :straight
 (flymake-ruff
  :type git
  :host github
  :repo "erickgnavar/flymake-ruff")
 :hook (eglot-managed-mode . flymake-ruff-load))


;; Initialize .dir-locals variables
(setq db/pname nil)
(setq db/plang nil)
(setq db/auto-isort nil)
(setq db/auto-blacken nil)
(setq db/test-command nil)
(setq db/black-config nil)

(defun db/setup-test-settings ()
  (when db/test-command
    (setq python-pytest-executable db/test-command)))

(defun db/configure-python-project ()
  (interactive)
  (progn
    (db/setup-test-settings)
    (db/configure-formatting)
    (db/enable-python-venv)))

(defun db/do-nothing ()
  nil)

(setq db/configure-fn-by-lang
      '(("python" . db/configure-python-project)
        (nil . db/do-nothing)))

(defun db/configure-formatting ()
  (when db/black-config
    (let ((black-config-path
           (concat (projectile-project-root) db/black-config)))
      (setf (alist-get 'black apheleia-formatters)
            `("black" "--config" ,black-config-path "-")))))

(defun db/configure-project ()
  (interactive)
  (let ((configure-fn (cdr (assoc db/plang db/configure-fn-by-lang))))
    (funcall configure-fn)))

;; Tests
(defun db/-project-rel-file-path ()
  (interactive)
  (string-remove-prefix (projectile-project-root) (buffer-file-name)))

(defun db/compose-django-tests-path ()
  (interactive)
  (replace-regexp-in-string
   "/" "."
   (string-remove-suffix ".py" (db/-project-rel-file-path))))

(defun db/-module-test-path ()
  (interactive)
  (which-function))

(defun db/compose-pytest-test-name ()
  (interactive)
  (kill-new
   (concat
    (db/-project-rel-file-path)
    "::"
    (replace-regexp-in-string "\\." "::" (db/-module-test-path)))))

(defun db/compose-django-test-name ()
  (interactive)
  (kill-new
   (concat
    (compose-django-file-test-path) "." (db/-module-test-path))))

(defun db/compose-pytest ()
  (interactive)
  (kill-new
   (concat
    python-pytest-executable " " (db/compose-pytest-test-name))))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'projectile-after-switch-project-hook 'db/configure-project)
