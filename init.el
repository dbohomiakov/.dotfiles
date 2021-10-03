;; KEEP .EMACS.D CLEAN!!!!
;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq
  user-emacs-directory
  (expand-file-name "~/.cache/emacs/")
  url-history-file
  (expand-file-name "url/history" user-emacs-directory))
;; Move to top to fix package-selected-package
;; see https://github.com/jwiegley/use-package/issues/397
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file t)

;; setup backup directory
(unless backup-directory-alist
  (setq backup-directory-alist
    `(("." . ,(concat user-emacs-directory "backups")))))
;; Reduce frequency of garbage collector
(setq gc-cons-threshold (* 200 1024 1024))
;; 1mb amount of data which Emacs reads from the process
(setq read-process-output-max (* 3 1024 1024))

;; Set the right directory to store the native comp cache
(add-to-list 'native-comp-eln-load-path
  (expand-file-name "eln-cache/" user-emacs-directory))
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

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering
  :straight (:host github :repo "emacscollective/no-littering"))

;; Get environment variables from shell
(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(when (daemonp)
  (exec-path-from-shell-initialize))

;; Load configs
(defvar db/config-folder "~/.emacs.d/configs/")

(defun db/load-config (config-filename)
  (load-file (concat db/config-folder config-filename ".el")))

(mapcar
  'db/load-config
  '
  ("common"
    "completition-in-minibuffer"
    "capf"
    "python"
    "project"
    "lsp"
    "org"
    "vcs"
    "ui"
    "kbd"
    "navigation"
    "clojure"
    "treemacs"
    "shell"
    "docker"
    "rust"
    "buffer"))
