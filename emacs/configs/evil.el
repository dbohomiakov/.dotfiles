(use-package
 evil
 :demand t
 :init
 (setq evil-want-C-u-scroll t)
 (setq evil-undo-system 'undo-redo)
 (setq evil-want-keybinding nil)
 (with-eval-after-load 'treemacs
   (define-key treemacs-mode-map (kbd "M-l") #'evil-window-right))
 :bind
 ("C-a" . evil-ex-search-forward)
 ("C-l" . evil-ex-nohighlight)
 ("M-h" . evil-window-left)
 ("M-l" . evil-window-right)
 ("M-k" . evil-window-up)
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
 (define-key evil-outer-text-objects-map "q" 'evil-a-double-quote)
 (define-key evil-inner-text-objects-map "q" 'evil-inner-double-quote)
 (define-key evil-outer-text-objects-map "q" 'evil-a-back-quote)
 (define-key evil-inner-text-objects-map "q" 'evil-inner-back-quote)

 (define-key evil-outer-text-objects-map "q" 'db/evil-a-quote)
 (define-key evil-inner-text-objects-map "q" 'db/evil-inner-quote)

 (define-key evil-inner-text-objects-map "g" #'db/evil-inner-paren)
 (define-key evil-outer-text-objects-map "g" #'db/evil-a-paren)

 (define-key
  evil-insert-state-map
  (kbd "C-h")
  'evil-delete-backward-char-and-join)
 (define-key
  evil-insert-state-map (kbd "M-h") 'evil-delete-backward-word)
 (define-key evil-insert-state-map (kbd "C-l") 'delete-char)
 (define-key evil-insert-state-map (kbd "M-l") 'kill-word)
 (evil-mode 1))


(use-package
 evil-textobj-tree-sitter
 :after evil
 :straight
 (evil-textobj-tree-sitter
  :type git
  :host github
  :repo "meain/evil-textobj-tree-sitter"
  :files (:defaults "queries" "treesit-queries"))
 :config
 ;; Define keys for different text objects
 ;; 'f' - function
 (define-key
  evil-outer-text-objects-map
  "f"
  (evil-textobj-tree-sitter-get-textobj "function.outer"))
 (define-key
  evil-inner-text-objects-map
  "f"
  (evil-textobj-tree-sitter-get-textobj "function.inner"))
 ;;
 (define-key
  evil-outer-text-objects-map
  "F"
  (evil-textobj-tree-sitter-get-textobj "call.outer"))
 (define-key
  evil-inner-text-objects-map
  "F"
  (evil-textobj-tree-sitter-get-textobj "call.inner"))
 ;; 'i' - import
 (define-key
  evil-inner-text-objects-map "i"
  (evil-textobj-tree-sitter-get-textobj
   "import.inner"
   '((python-mode . [(import_statement) @import.inner])
     (rust-mode . [(use_declaration) @import.inner]))))
 ;; 'm' - import from
 (define-key
  evil-inner-text-objects-map "I"
  (evil-textobj-tree-sitter-get-textobj
   "import_from.inner"
   '((python-mode . [(import_from_statement) @import_from.inner])
     (rust-mode . [(use_declaration) @import_from.inner]))))
 ;; 'c' - class
 (define-key
  evil-outer-text-objects-map
  "c"
  (evil-textobj-tree-sitter-get-textobj "class.outer"))
 (define-key
  evil-inner-text-objects-map
  "c"
  (evil-textobj-tree-sitter-get-textobj "class.inner"))
 ;; 'e' - expressino statement
 (define-key
  evil-outer-text-objects-map
  "e"
  (evil-textobj-tree-sitter-get-textobj "statement.outer"))

 ;;------------------ GOTO --------------------------
 ;; -------------------Function------------------------
 ;; Goto start of next function
 (define-key
  evil-normal-state-map (kbd "]f")
  (lambda ()
    (interactive)
    (evil-textobj-tree-sitter-goto-textobj "function.outer")))
 ;; Goto start of previous function
 (define-key
  evil-normal-state-map (kbd "[f")
  (lambda ()
    (interactive)
    (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))
 ;; Goto end of next function
 (define-key
  evil-normal-state-map (kbd "]F")
  (lambda ()
    (interactive)
    (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t)))
 ;; Goto end of previous function
 (define-key
  evil-normal-state-map (kbd "[F")
  (lambda ()
    (interactive)
    (evil-textobj-tree-sitter-goto-textobj "function.outer" t t)))

 ;; -------------------Class------------------------
 ;; Goto start of next class
 (define-key
  evil-normal-state-map (kbd "]c")
  (lambda ()
    (interactive)
    (evil-textobj-tree-sitter-goto-textobj "class.outer")))
 ;; Goto start of previous function
 (define-key
  evil-normal-state-map (kbd "[c")
  (lambda ()
    (interactive)
    (evil-textobj-tree-sitter-goto-textobj "class.outer" t)))
 ;; Goto end of next function
 (define-key
  evil-normal-state-map (kbd "]C")
  (lambda ()
    (interactive)
    (evil-textobj-tree-sitter-goto-textobj "class.outer" nil t)))
 'sfsfsf'
 ;; Goto end of previous function
 (define-key
  evil-normal-state-map (kbd "[C")
  (lambda ()
    (interactive)
    (evil-textobj-tree-sitter-goto-textobj "class.outer" t t))))


(use-package
 evil-numbers
 :config
 (define-key
  evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
 (define-key
  evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt))

(use-package
 evil-collection
 :demand t
 :after evil
 :ensure t
 :config
 (setq evil-want-keybinding nil)
 (evil-collection-init))

(use-package
 evil-surround
 :ensure t
 :after evil
 :config
 ;; Define q/Q as text-object for quotes
 (setq-default evil-surround-pairs-alist
               (append
                '((?q . ("'" . "'")) (?Q . ("\"" . "\"")))
                evil-surround-pairs-alist))
 (global-evil-surround-mode 1))

(use-package
 evil-matchit
 :after evil
 :init (global-evil-matchit-mode 1))

(use-package
 evil-smartparens
 :after evil
 :hook
 (clojure-mode . evil-smartparens-mode)
 (emacs-lisp-mode . evil-smartparens-mode)
 (python-mode . evil-smartparens-mode))

(use-package evil-exchange :after evil)

(use-package evil-visualstar :config (global-evil-visualstar-mode))

(use-package
 evil-paredit
 :disabled
 :hook
 (python-mode . evil-paredit-mode)
 (emacs-lisp-mode . evil-paredit-mode))
