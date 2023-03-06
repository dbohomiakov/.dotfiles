(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
;; Evil mode
;; Rebind Ctrl+g to escape
(global-set-key (kbd "<escape>")      'keyboard-escape-quit)
;; Rebind universal argument
(global-set-key (kbd "C-M-u") 'universal-argument)

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-undo-system 'undo-redo)
  ;; :bind ("C-s" . evil-ex-search-forward)
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
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (evil-mode 1))

;; Use visual line motions even outside of visual-line-mode buffers
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

(use-package evil-textobj-tree-sitter
  :after evil
  :config
  ;; Define keys for different text objects
  ;; 'f' - function
  (define-key evil-outer-text-objects-map "f"
    (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f"
    (evil-textobj-tree-sitter-get-textobj "function.inner"))
  ;; 'i' - import
  (define-key evil-inner-text-objects-map "i" (evil-textobj-tree-sitter-get-textobj "import.inner"
                                              '((python-mode . [(import_statement) @import.inner])
                                                (rust-mode . [(use_declaration) @import.inner]))))
  ;; 'm' - import from
  (define-key evil-inner-text-objects-map "I" (evil-textobj-tree-sitter-get-textobj "import_from.inner"
                                              '((python-mode . [(import_from_statement) @import_from.inner])
                                                (rust-mode . [(use_declaration) @import_from.inner]))))
  ;; 'c' - class
  (define-key evil-outer-text-objects-map "c"
    (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key evil-inner-text-objects-map "c"
    (evil-textobj-tree-sitter-get-textobj "class.inner"))
  ;; 'e' - expressino statement
  (define-key evil-outer-text-objects-map "e"
    (evil-textobj-tree-sitter-get-textobj "statement.outer")))

(use-package evil-numbers
  :config
  (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt))

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
  "pf"  'project-find-file
  "ps"  'projectile-switch-project
  "pg"  'consult-ripgrep
  "pd"  'projectile-dired
  "pp"  'consult-projectile
  "ff"  'find-file
  "\\"  'evil-window-vsplit
  "-"   'evil-window-split
  "s"   'save-buffer
  "k"   'save-buffers-kill-terminal
  "b"   'persp-switch-to-buffer
  "ef"  'eval-defun
  "er"  'eval-region
  "eb"  'eval-buffer
  ;; ABBR stands for git
  "gs"  'magit-status
  "gb"  'magit-blame
  "gd"  'magit-diff
  "gu"  'git-link
  "gc"  'magit-branch-checkout
  ;; ORG
  "oe"  'org-babel-execute-src-block
  "or"  'org-babel-remove-result-one-or-many
  ;; KMACRO
  "mq"   'kmacro-start-macro
  "mQ"   'kmacro-end-macro
  "mn"   'kmacro-name-last-macro
  "me"   'kmacro-call-macro
  "tt"  'treemacs
  "tf" 'transpose-frame
  "tr" 'rotate-frame
  "tr" 'rotate-frame-clockwise
  "tl" 'rotate-frame-anclockwise
  "th" 'flop-frame
  "tv" 'flip-frame
  "w"  'ace-window
  "d"  'ace-delete-window
  "rb" 'revert-buffer
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
(evil-define-key 'normal 'global (kbd "gs") #'evil-ex-sort)
;; Folding keys
(evil-define-key 'normal 'global (kbd "ghh") #'hs-toggle-hiding)
(evil-define-key 'normal 'global (kbd "ghc") #'hs-hide-all)
(evil-define-key 'normal 'global (kbd "gho") #'hs-show-all)
(evil-define-key 'normal 'global (kbd "ghl") #'hs-hide-level)
;; Multiple cursor (redefine default key sequences)
(evil-define-key '(normal visual) 'global (kbd "gm") evil-mc-cursors-map)
;; CamelCase/snake_case switch
(evil-define-key 'normal 'global (kbd "gc") #'db/string-inflection-cycle-auto)
(use-package evil-exchange)

;; Use q to quit the read-only buffers
(defun db/evil-record-macro ()
  (interactive)
  (if buffer-read-only
      (quit-window)
    (call-interactively 'evil-record-macro)))

(with-eval-after-load 'evil-maps
  (define-key evil-normal-state-map (kbd "q") 'db/evil-record-macro))

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

(global-set-key [f5] 'db/toggle-theme)
