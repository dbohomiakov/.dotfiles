;;;  -*- lexical-binding: t -*-
(defun set-display-fill-column-indicator (num)
  (progn
    (setq-default display-fill-column-indicator-column num)))

(add-hook 'python-mode-hook 'display-fill-column-indicator-mode)

(defun fix-python-indent ()
  (setq python-indent-offset 4)
  (setq tab-width 4)
  (subword-mode 1))

(use-package python-mode
  ;; :bind ("TAB" . company-indent-or-complete-common)
  :hook ((python-mode . fix-python-indent)
         (python-mode . (lambda ()
                               (set-display-fill-column-indicator 88)))
         (python-mode . hs-minor-mode)
         (python-mode . yas-minor-mode)
         (python-mode . abbrev-mode)))

(use-package python-pytest
  :init
  (setq pytest-cmd-flags "--pdbcls=IPython.terminal.debugger:Pdb")
  :custom
  (python-pytest-confirm t)
  (python-pytest-pdb-track t))

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
    ))

(defun db/do-nothing () nil)

(setq db/configure-fn-by-lang
  '(("python" . db/configure-python-project) (nil . db/do-nothing)))

(defun db/configure-formatting ()
  (when db/black-config
    (let ((black-config-path (concat (projectile-project-root)
                                     db/black-config)))
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

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'projectile-after-switch-project-hook 'db/configure-project)