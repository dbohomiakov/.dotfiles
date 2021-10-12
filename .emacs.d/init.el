;;;;; Startup optimizations
;;;;;; Set garbage collection threshold
;; From https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 100))
;;;;;; Set file-name-handler-alist
;; Also from https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
;;;;;; Set deferred timer to reset them
(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold gc-cons-threshold-original)
   (setq file-name-handler-alist file-name-handler-alist-original)
   (makunbound 'gc-cons-threshold-original)
   (makunbound 'file-name-handler-alist-original)
   (message "gc-cons-threshold and file-name-handler-alist restored")))
;;;;;;;;;;;;;;;;;;;

;; 3mb amount of data which Emacs reads from the process
(setq read-process-output-max (* 1 1024 1024))
;; Measure startup time
(defun db/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'db/display-startup-time)

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

;; Profiling
(use-package esup
  :ensure t)

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

(mapcar 'db/load-config
        '(
            "common"
            "completition-in-minibuffer"
            "capf"
            "python"
            "project"
            "lsp"
            "org"
            "vcs"
            "ui"
            "navigation"
            "clojure"
            "treemacs"
            "shell"
            "docker"
            "rust"
            "buffer"
            ;; "kbd" should be the last one cause uses defined in configs above variables/function etc.
            "kbd"))
