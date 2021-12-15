(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
;; Evil mode
;; Rebind Ctrl+g to escape
(global-set-key (kbd "<escape>")      'keyboard-escape-quit)

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-undo-system 'undo-redo)
  :config
  ;; remove evil-digraph binding to use "C-k" for
  ;; vertical movement in insert mode in popups
  (evil-update-insert-state-bindings "C-k" t t)
  ;; Define q/Q as text-object for quotes
  (define-key evil-outer-text-objects-map "q" 'evil-a-single-quote)
  (define-key evil-inner-text-objects-map "q" 'evil-inner-single-quote)
  (define-key evil-outer-text-objects-map "Q" 'evil-a-double-quote)
  (define-key evil-inner-text-objects-map "Q" 'evil-inner-double-quote)
  (define-key evil-outer-text-objects-map "Q" 'evil-a-back-quote)
  (define-key evil-inner-text-objects-map "Q" 'evil-inner-back-quote)
  (evil-mode 1))

(use-package evil-textobj-tree-sitter
  :after evil
  :config
  ;; Define keys for different text objects
  ;; 'f' - function
  (define-key evil-outer-text-objects-map "f"
    (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f"
    (evil-textobj-tree-sitter-get-textobj "function.inner"))
  ;; 'm' - import
  (define-key evil-outer-text-objects-map "i" (evil-textobj-tree-sitter-get-textobj "import"
                                              '((python-mode . [(import_statement) @import])
                                                (rust-mode . [(use_declaration) @import]))))
  (define-key evil-inner-text-objects-map "i" (evil-textobj-tree-sitter-get-textobj "import"
                                              '((python-mode . [(import_statement) @import])
                                                (rust-mode . [(use_declaration) @import]))))
  (define-key evil-outer-text-objects-map "i" (evil-textobj-tree-sitter-get-textobj "import"
                                              '((python-mode . [(import_from_statement) @import])
                                                (rust-mode . [(use_declaration) @import]))))
  (define-key evil-inner-text-objects-map "i" (evil-textobj-tree-sitter-get-textobj "import"
                                              '((python-mode . [(import_from_statement) @import])
                                                (rust-mode . [(use_declaration) @import]))))
  ;; 'c' - class
  (define-key evil-outer-text-objects-map "c"
    (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key evil-inner-text-objects-map "c"
    (evil-textobj-tree-sitter-get-textobj "class.inner"))
  ;; 'e' - expressino statement
  (define-key evil-outer-text-objects-map "e"
    (evil-textobj-tree-sitter-get-textobj "statement.outer"))
  )


(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  ;; Define q/Q as text-object for quotes
  (setq-default evil-surround-pairs-alist
                (append '((?q . ("'" . "'"))
                          (?Q . ("\"" . "\"")))
                        evil-surround-pairs-alist))
  (global-evil-surround-mode 1))

(use-package evil-matchit
  :after evil
  :init
  (global-evil-matchit-mode 1))

(use-package evil-smartparens
  :after evil
  :hook
  (clojure-mode . evil-smartparens-mode)
  (emacs-lisp-mode . evil-smartparens-mode)
  (python-mode . evil-smartparens-mode))

(use-package key-chord
  :init
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-visual-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
  (key-chord-define evil-visual-state-map "kj" 'evil-normal-state))

(use-package general
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
  "\\"  'evil-window-vsplit
  "-"   'evil-window-split
  "s"   'save-buffer
  "k"   'save-buffers-kill-terminal
  "b"   'persp-switch-to-buffer
  "ef"  'eval-defun
  ;; ABBR stands for git
  "gs"  'magit-status
  "gb"  'magit-blame
  "gd"  'magit-diff
  "gu"  'browse-at-remote
  "gc"  'magit-branch-checkout
  "er"  'eval-region
  "eb"  'eval-buffer
  ;; "tt"  'treemacs
  ;; "tw"  'delete-trailing-whitespace
  ;; "tl"  'delete-trailing-lines
  "tf" 'transpose-frame
  "tr" 'rotate-frame
  "tr" 'rotate-frame-clockwise
  "tl" 'rotate-frame-anclockwise
  "th" 'flop-frame
  "tv" 'flip-frame
  "w"  'ace-window
  "d"  'ace-delete-window
  "rb" 'revert-buffer
  "nw" 'db/to-and-from-minibuffer
  "nj" 'db/down-from-outside
  "nk" 'db/up-from-outside
  "ht" 'hs-toggle-hiding
  "hal" 'hs-hide-all
  "has" 'hs-show-all
  "hl" 'hs-hide-level
  "'"  'expand-abbrev
  "\/"  'unexpand-abbrev)

;; Use consult-line for incremental search
(evil-define-key 'normal 'global (kbd "/") #'consult-line)
(evil-define-key 'visual 'global (kbd "/") #'consult-line)
(evil-define-key 'normal 'global (kbd "?") #'avy-goto-char-2)
(evil-define-key 'normal 'visual (kbd "?") #'avy-goto-char-2)
(evil-define-key 'normal 'global (kbd "gs") #'avy-goto-char-2)
;; Folding keys
(evil-define-key 'normal 'global (kbd "ghh") #'hs-toggle-hiding)
(evil-define-key 'normal 'global (kbd "ghc") #'hs-hide-all)
(evil-define-key 'normal 'global (kbd "gho") #'hs-show-all)
(evil-define-key 'normal 'global (kbd "ghl") #'hs-hide-level)
;; Multiple cursor (redefine default key sequences)
(evil-define-key '(normal visual) 'global (kbd "gm") evil-mc-cursors-map)

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
