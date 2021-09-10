;; keep .emacs.d CLEAN!!!!
;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/") url-history-file (expand-file-name
                                                                                  "url/history"
                                                                                  user-emacs-directory))
(setq comp-deferred-compilation-deny-list nil)
;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)
;; Set the right directory to store the native comp cache
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
;; Silence compiler warnings as they can be pretty disruptive
(setq comp-async-report-warnings-errors nil)
(setq comp-deferred-compilation t)

;; Bootstrap `straight.el'
(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el"
                                        user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer (url-retrieve-synchronously
                          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                          'silent 'inhibit-cookies)
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
(use-package
  no-littering
  :straight (:host github
                   :repo "emacscollective/no-littering"))

;; Keep customization settings in a temporary file
(setq custom-file (if (boundp 'server-socket-dir)
                      (expand-file-name "custom.el" server-socket-dir)
                    (expand-file-name (format "emacs-custom-%s.el" (user-uid))
                                      temporary-file-directory)))
(load custom-file t)
;; Move to top to fix package-selected-package
;; see https://github.com/jwiegley/use-package/issues/397
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; Get environment variables from shell
(use-package
  exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(when (daemonp)
  (exec-path-from-shell-initialize))

;; Common
(use-package
  better-defaults)

;; Load configs
(defvar base-config-folder "~/.emacs.d/configs/")
(defvar python-config (concat base-config-folder "python-cfg.el"))
(defvar project-config (concat base-config-folder "project-cfg.el"))
(defvar lsp-config (concat base-config-folder "lsp-cfg.el"))
(defvar autocomplete-config (concat base-config-folder "autocomplete-cfg.el"))
(defvar org-config (concat base-config-folder "org-cfg.el"))
(defvar vcs-config (concat base-config-folder "vcs-cfg.el"))
(defvar ui-config (concat base-config-folder "ui-cfg.el"))
(defvar kbd-config (concat base-config-folder "kbd-cfg.el"))
(defvar navigation-config (concat base-config-folder "navigation-cfg.el"))
(defvar clojure-config (concat base-config-folder "clojure-cfg.el"))
(defvar treemacs-config (concat base-config-folder "treemacs-cfg.el"))
(defvar shell-config (concat base-config-folder "shell-cfg.el"))
(defvar docker-config (concat base-config-folder "docker-cfg.el"))
(defvar rust-config (concat base-config-folder "rust-cfg.el"))
(defvar buffer-config (concat base-config-folder "buffer-cfg.el"))
(load-file autocomplete-config)
(load-file python-config)
(load-file project-config)
(load-file lsp-config)
(load-file org-config)
(load-file vcs-config)
(load-file ui-config)
(load-file kbd-config)
(load-file navigation-config)
(load-file clojure-config)
(load-file treemacs-config)
(load-file shell-config)
(load-file docker-config)
(load-file rust-config)
(load-file buffer-config)
;; General settings

;; Eval elisp functions
(global-set-key (kbd "M-:") 'pp-eval-expression)

;; General
(setq inhibit-startup-screen t)

;; Cursor settings
(set-cursor-color "#f6f6f6")
(blink-cursor-mode -1)                  ; Disable blinking cursor

(setq gc-cons-threshold (* 200 1024 1024)) ; Reduce frequency of garbage collector
(setq read-process-output-max (* 3 1024 1024)) ;; 1mb amount of data which Emacs reads from the process
(scroll-bar-mode -1)                    ; Disable visible scrollbar
(tool-bar-mode -1)                      ; Disable the toolbar
(tooltip-mode -1)                       ; Disable tooltip
(set-fringe-mode 10)                    ; Give some brithing room
(menu-bar-mode -1)                      ; Disable menu bar
(setq visible-bell nil)                 ; Enable visible bell

;; nice scrolling
(setq scroll-margin 0 scroll-preserve-screen-position 1)

(global-display-line-numbers-mode)     ; enable showing lines numbers
(setq column-number-mode t)            ; enable showing column numbers
(setq size-indication-mode t)
(setq require-final-newline t)  ; Newline at end of file
(delete-selection-mode t)       ; Delete the selection with a keypress
(global-auto-revert-mode t) ; revert buffers automatically when underlying files are changed externally

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Enable case-switching
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq tab-always-indent 'complete) ;; smart tab behavior - indent or complete

;; Move blocks of code horizontaly
(use-package
  drag-stuff
  :config (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

;; Commenter
(use-package
  evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

;; optional if you want which-key integration
(use-package
  which-key
  :config (which-key-mode))

;; Use mode for debugging command and keys entered
(use-package
  command-log-mode)

(use-package
  tramp)

;; Ask 'y' or 'n' for closing buffer
(fset 'yes-or-no-p 'y-or-n-p)

;; Multiple cursors
(use-package
  multiple-cursors
  :bind ("C-c m c" . 'mc/edit-lines))

;; Higlight braces
(use-package
  paren
  :config (set-face-attribute 'show-paren-match-expression nil
                              :background "#363e4a")
  (show-paren-mode 1))

(use-package
  ibuffer-vc
  :init (setq ibuffer-vc t))

(use-package
  smartparens
  :config (smartparens-global-mode 1))

(use-package
  yasnippet)

(use-package
  json-mode)

(use-package
  highlight-indent-guides
  :custom (highlight-indent-guides-method 'bitmap)
  :hook (emacs-lisp-mode . highlight-indent-guides-mode))

(use-package
  markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "/usr/bin/pandoc"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#002b36" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198"
                            "#839496"])
 '(company-idle-delay 0.5)
 '(company-minimum-prefix-length 1)
 '(custom-safe-themes (quote ("b35b973fd042033414eb2ab6f38f49b375ecf8e538eca71e40a92eea5022e636"
                              "8d7684de9abb5a770fbfd72a14506d6b4add9a7d30942c6285f020d41d76e0fa"
                              "4a8d4375d90a7051115db94ed40e9abb2c0766e80e228ecad60e06b3b397acab"
                              "c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19"
                              "ac07a8c87b4958c65facfcb2e041d57f24e0d5702a9d540e44ff8c57bd8185d6"
                              "a257f0b8b7bc1a4310e7dc4ccbd5587439644b4f56b5f2672863ca0afbf3e4c1"
                              "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26"
                              default)))
 '(fci-rule-color "#405A61")
 '(jdee-db-active-breakpoint-face-colors (cons "#073642" "#268bd2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#073642" "#859900"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#073642" "#56697A"))
 '(objed-cursor-color "#dc322f")
 '(org-bullets-bullet-list (quote ("◉" "○" "●" "○" "●" "○" "●")) t)
 '(package-selected-packages (quote (ivy-rich smex flx evil-nerd-commenter org-bullets which-key
                                              use-package pyenv-mode projectile material-theme
                                              lsp-ui helm-lsp flycheck dap-mode company-jedi
                                              company-anaconda)))
 '(pdf-view-midnight-colors (cons "#839496" "#002b36"))
 '(projectile-completion-system (quote ivy))
 '(rustic-ansi-faces ["#002b36" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198"
                      "#839496"])
 '(vc-annotate-background "#002b36")
 '(vc-annotate-color-map (list (cons 20 "#859900")
                               (cons 40 "#959300")
                               (cons 60 "#a58e00")
                               (cons 80 "#b58900")
                               (cons 100 "#bc7407")
                               (cons 120 "#c35f0e")
                               (cons 140 "#cb4b16")
                               (cons 160 "#cd4439")
                               (cons 180 "#d03d5d")
                               (cons 200 "#d33682")
                               (cons 220 "#d63466")
                               (cons 240 "#d9334a")
                               (cons 260 "#dc322f")
                               (cons 280 "#ba3f41")
                               (cons 300 "#994d54")
                               (cons 320 "#775b67")
                               (cons 340 "#405A61")
                               (cons 360 "#405A61")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
