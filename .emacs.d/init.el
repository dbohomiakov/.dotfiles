(defvar db/emacs-dir (or (getenv "EMACSDIR") "~/.emacs.d"))
(add-to-list 'load-path (concat db/emacs-dir "/custom-scripts"))

;; 3mb amount of data which Emacs reads from the process
(setq read-process-output-max (* 3 1024 1024))
;; Measure startup time
(defun db/display-startup-time ()
(message "Emacs loaded in %s with %d garbage collections."
            (format "%.2f seconds"
                    (float-time
                    (time-subtract after-init-time before-init-time)))
            gcs-done))

(add-hook 'emacs-startup-hook #'db/display-startup-time)

;; run smart garbage collection
(require 'gcmh)
(gcmh-mode 1)

;; KEEP .EMACS.D CLEAN!!!!
(load custom-file t)

;; setup backup directory
(unless backup-directory-alist
(setq backup-directory-alist
        `(("." . ,(concat user-emacs-directory "backups")))))

(setq comp-deferred-compilation-deny-list nil)
;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)
;; Silence compiler warnings as they can be pretty disruptive
(setq comp-async-report-warnings-errors nil)
(setq comp-deferred-compilation t)

;; Bootstrap `straight.el'
(defvar bootstrap-version)
(let
    (
    (bootstrap-file
    (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        user-emacs-directory))
    (bootstrap-version 5))
(unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent
        'inhibit-cookies)
    (goto-char (point-max))
    (eval-print-last-sexp)))
(load bootstrap-file nil 'nomessage))
;; Use straight as default manager in use-package declarations
(setq straight-use-package-by-default t)
;; Download and set up use-package
(straight-use-package 'use-package)
;; Helpers
(require 'straight-x)
(setq use-package-always-ensure t)

(setq redisplay-dont-pause t)

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering
  :straight (:host github :repo "emacscollective/no-littering"))

;; Profiling
(use-package esup
  :defer t)

(use-package bug-hunter
  :defer t)

;; Get environment variables from shell
(use-package exec-path-from-shell)

(when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
(when (daemonp)
    (exec-path-from-shell-initialize))

(defvar db/config-folder (concat db/emacs-dir "/configs/"))
(defun db/load-config (config-filename)
(load-file (concat db/config-folder config-filename ".el")))

(mapcar 'db/load-config
        '(
        "common"
        "evil"
        "completition-in-minibuffer"
        "capf"
        "org"
        "python"
        "go"
        "project"
        "lsp"
        ;; "eglot"
        "vcs"
        "ui"
        "navigation"
        "clojure"
        "treemacs"
        ;; "shell"
        "docker"
        "rust"
        "buffer"
        "tree-sitter"
        "http"
        ;; "kbd" should be the last one cause uses defined in configs above variables/function etc.
        "kbd"))
