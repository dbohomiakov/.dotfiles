(use-package
 eglot
 :straight (:type built-in)
 :init
 (setq
  eglot-stay-out-of '(company flymake)
  read-process-output-max (* 1024 1024))
 :hook
 ((python-mode . eglot-ensure)
  (rust-mode . eglot-ensure)
  (go-mode . eglot-ensure))
 :custom
 (eglot-ignored-server-capabilities
  '(:hoverProvider :documentHighlightProvider))
 (eglot-events-buffer-size 0)
 (eglot-sync-connect 1)
 (eglot-connect-timeout 10)
 (eglot-extend-to-xref t)
 (eglot-send-changes-idle-time 0.5)
 (eglot-auto-display-help-buffer nil)
 (eglot-autoshutdown t))

(use-package
 eglot-booster
 :straight
 (elgot-booster :type git :host github :repo "jdtsmith/eglot-booster")
 :after eglot
 :config (eglot-booster-mode))

(setq eldoc-idle-delay 0.75)
(setq company-idle-delay 0.75)
(setq flymake-no-changes-timeout 0.5)


(with-eval-after-load 'eglot
  (add-to-list
   'eglot-server-programs
   '((python-mode)
     .
     ("/home/dmytro/.asdf/shims/pyright-langserver" "--stdio"))))

(use-package consult-eglot :custom (consult-eglot-ignore-column t))
;
(require 'eldoc-box)
;
(add-hook 'python-base-mode-hook 'flymake-mode)
;; (setq python-flymake-command
;;       '("ruff" "--quiet" "--stdin-filename=stdin" "-"))
;
;; (add-hook
;;  'eglot-managed-mode-hook
;;  (lambda ()
;;    (cond
;;     ((derived-mode-p 'python-base-mode)
;;      (add-hook 'flymake-diagnostic-functions 'python-flymake nil t))
;;     ;; if not adding diagnostic functions to other modes just use an if
;;     ;; ...
;;     (t
;;      nil))))

(defun my-filter-eglot-diagnostics (diags)
  "Drop Pyright 'variable not accessed' notes from DIAGS."
  (list
   (seq-remove
    (lambda (d)
      (and (eq (flymake-diagnostic-type d) 'eglot-note)
           (s-starts-with? "Pyright:" (flymake-diagnostic-text d))
           (s-ends-with?
            "is not accessed" (flymake-diagnostic-text d))))
    (car diags))))

(advice-add
 'eglot--report-to-flymake
 :filter-args #'my-filter-eglot-diagnostics)

(use-package
 breadcrumb
 :straight
 (breadcrumb :type git :host github :repo "joaotavora/breadcrumb")
 :hook
 (python-mode . breadcrumb-mode)
 (go-mode . breadcrumb-mode)
 (rust-mode . breadcrumb-mode))

(defun my/eglot-capf ()
  (setq-local completion-at-point-functions
              (list
               (cape-capf-super
                (cape-capf-buster #'eglot-completion-at-point)
                #'cape-dabbrev
                ;; (cape-company-to-capf #'company-yasnippet)
                ))))

(add-hook 'eglot-managed-mode-hook #'my/eglot-capf)

;; (use-package
;;  flycheck-eglot
;;  :ensure t
;;  :after (flycheck eglot)
;;  :custom (flycheck-eglot-exclusive nil)
;;  :config (global-flycheck-eglot-mode 1))

(use-package
 dape
 ;; Currently only on github
 :straight (dape :type git :host github :repo "svaante/dape")
 :config
 ;; Add inline variable hints, this feature is highly experimental
 ;; (setq dape-inline-variables t)

 ;; To remove info buffer on startup
 ;; (remove-hook 'dape-on-start-hooks 'dape-info)

 ;; To remove repl buffer on startup
 ;; (remove-hook 'dape-on-start-hooks 'dape-repl)

 ;; By default dape uses gdb keybinding prefix
 ;; (setq dape-key-prefix "\C-x\C-a")

 ;; Use n for next etc. in REPL
 ;; (setq dape-repl-use-shorthand t)

 ;; Kill compile buffer on build success
 ;; (add-hook 'dape-compile-compile-hooks 'kill-buffer)

 ;; Projectile users
 (setq dape-cwd-fn 'projectile-project-root)

 (add-to-list
  'dape-configs
  `(debugpy
    modes
    (python-ts-mode python-mode)
    command
    "python3"
    command-args
    ("-m" "debugpy.adapter")
    :type "executable"
    :request "launch"
    :cwd dape-cwd-fn
    :program dape-find-file-buffer-default))

 (add-to-list
  'dape-configs
  `(delve
    modes
    (go-mode go-ts-mode)
    command
    "dlv"
    command-args
    ("dap" "--listen" "127.0.0.1:55878")
    command-cwd
    dape-cwd-fn
    host
    "127.0.0.1"
    port
    55878
    :type "debug"
    :request "launch"
    :cwd dape-cwd-fn
    :program dape-cwd-fn)))
