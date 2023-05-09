(use-package eglot
  :init
  (setq eglot-stay-out-of '(company flymake)
        read-process-output-max (* 1024 1024))
  :hook
  ((python-mode . eglot-ensure)
   (rust-mode . eglot-ensure)
   (go-mode . eglot-ensure))
  :custom
  (eglot-events-buffer-size 0)
  (eglot-sync-connect 1)
  (eglot-connect-timeout 10)
  (eglot-extend-to-xref t)
  (eglot-send-changes-idle-time 0.5)
  (eglot-auto-display-help-buffer nil)
  (eglot-autoshutdown t))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("/home/dbohomiakov/.asdf/installs/nodejs/13.13.0/.npm/bin/pyright-langserver" "--stdio"))))

(use-package consult-eglot
  :custom
  (consult-eglot-ignore-column t))

(require 'eldoc-box)
