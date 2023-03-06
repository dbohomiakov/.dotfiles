(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-undo-system 'undo-redo)
  (setq evil-want-keybinding nil)
  :bind ("C-a" . evil-ex-search-forward)
  ("C-l" . evil-ex-nohighlight)
  ("M-h" . evil-window-left)
  ("M-l" . evil-window-right)
  ("M-k" . evil-window-top)
  ("M-j" . evil-window-down)
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
  :ensure t
  :config
  (setq evil-want-keybinding nil)
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

(use-package evil-exchange
  :after evil)
