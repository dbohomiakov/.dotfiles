;; Evil mode
;; Rebind Ctrl+g to escape
(global-set-key (kbd "<escape>")      'keyboard-escape-quit)

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  ;; remove evil-digraph binding to use "C-k" for
  ;; vertical movement in insert mode in popups
  (evil-update-insert-state-bindings "C-k" t t)
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
  (global-undo-tree-mode)
  (evil-set-undo-system 'undo-tree))

(use-package
  general
  :after evil-collection
  :config (general-evil-setup t)
  (general-create-definer db/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC"))

(db/leader-keys
  "pf"  'projectile-find-file
  "ps"  'projectile-switch-project
  "pg"  'consult-ripgrep
  "pc"  'projectile-compile-project
  "pd"  'projectile-dired
  "pp"  'consult-projectile
  "ff"  'find-file
  "\\"   'split-window-horizontally
  "-"   'split-window-vertically
  "s"   'save-buffer
  "k"   'save-buffers-kill-terminal
  "b"   'consult-buffer
  "ef"  'eval-defun
  "ms"  'magit-status
  "mb"  'magit-blame
  "md"  'magit-diff
  "mu"  'browse-at-remote
  "mc"  'magit-branch-checkout
  "er"  'eval-region
  "eb"  'eval-buffer
  "tt"  'treemacs
  "tw"  'delete-trailing-whitespace
  "tl"  'delete-trailing-lines
  "w"  'ace-window
  "d"  'ace-delete-window
  "rb" 'revert-buffer
  "nw" 'db/to-and-from-minibuffer
  "nj" 'db/down-from-outside
  "nk" 'db/up-from-outside
  "ht" 'hs-toggle-hiding
  "hal" 'hs-hide-all
  "has" 'hs-show-all
  "hl" 'hs-hide-level)

;; Use consult-line for incremental search
(evil-define-key 'normal 'global (kbd "/") #'consult-line)
(evil-define-key 'visual 'global (kbd "/") #'consult-line)
(evil-define-key 'normal 'global (kbd "ghh") #'hs-toggle-hiding)
(evil-define-key 'normal 'global (kbd "ghc") #'hs-hide-all)
(evil-define-key 'normal 'global (kbd "gho") #'hs-show-all)
(evil-define-key 'normal 'global (kbd "ghl") #'hs-hide-level)

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
