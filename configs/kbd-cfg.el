;; Evil mode
;; Rebind Ctrl+g to escape
(global-set-key (kbd "<escape>")      'keyboard-escape-quit)

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit)
(global-evil-matchit-mode 1)

(use-package evil-smartparens
  :after evil
  :hook
  (clojure-mode . evil-smartparens-mode)
  (emacs-lisp-mode . evil-smartparens-mode))

(use-package key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-visual-state-map "jk" 'evil-normal-state)
(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
(key-chord-define evil-visual-state-map "kj" 'evil-normal-state)

(use-package undo-tree
  :init
  (global-undo-tree-mode))

(evil-set-undo-system 'undo-tree)

(use-package
  general
  :after evil-collection
  :config (general-evil-setup t)
  (general-create-definer db/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC"))

;; Use swiper for incremental search
(evil-define-key 'normal 'global (kbd "/") #'counsel-grep-or-swiper)
(evil-define-key 'visual 'global (kbd "/") #'counsel-grep-or-swiper)

(db/leader-keys
  "pf"  'counsel-projectile-find-file
  "ps"  'projectile-switch-project
  "pg"  'counsel-projectile-rg
  "pp"  'counsel-projectile
  "pc"  'projectile-compile-project
  "pd"  'projectile-dired
  "ff"  'find-file
  "\\"   'split-window-horizontally
  "-"   'split-window-vertically
  "s"   'save-buffer
  "k"   'save-buffers-kill-terminal
  "b"   'ivy-switch-buffer
  "ef"  'eval-defun
  "ms"  'magit-status
  "mb"  'magit-blame
  "md"  'magit-diff
  "er"  'eval-region
  "eb"  'eval-buffer
  "tt"  'treemacs
  "tw"  'delete-trailing-whitespace
  "tl"  'delete-trailing-lines
  "w"  'ace-window
  "d"  'ace-delete-window
  "rb" 'revert-buffer)

(use-package evil-exchange)

;; Use q to quit the read-only buffers
(defun my-evil-record-macro ()
  (interactive)
  (if buffer-read-only
      (quit-window)
    (call-interactively 'evil-record-macro)))

(with-eval-after-load 'evil-maps
  (define-key evil-normal-state-map (kbd "q") 'my-evil-record-macro))

;; Disable copy with selection
(fset 'evil-visual-update-x-selection 'ignore)

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))
