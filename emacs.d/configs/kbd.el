(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
;; Evil mode
;; Rebind Ctrl+g to escape
(global-set-key (kbd "<escape>")      'keyboard-escape-quit)
;; Rebind universal argument
(global-set-key (kbd "C-M-u") 'universal-argument)

;; use visual line motions even outside of visual-line-mode buffers
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

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
  ;; Project
  "pf"  'project-find-file
  "ps"  'projectile-switch-project
  "pg"  'consult-ripgrep
  "pd"  'project-dired
  "pp"  'consult-projectile
  "ff"  'find-file

  "\\"  'evil-window-vsplit
  "-"   'evil-window-split
  "s"   'save-buffer
  "d"   'evil-window-delete
  "k"   'save-buffers-kill-terminal
  "b"   'consult-buffer

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
  "oe"  'org-ctrl-c-ctrl-c
  "or"  'org-babel-remove-result-one-or-many
  ;; KMACRO
  "mq"   'kmacro-start-macro
  "mQ"   'kmacro-end-macro
  "mn"   'kmacro-name-last-macro
  "me"   'kmacro-call-macro
  "tt"  'dirvish-side
  ;; Evil window
  "wn"  'evil-window-new
  "wo"  'delete-other-windows
  "ww"  'other-window
  "wd"  'ace-delete-window

  ;; Folding
  "ft" 'ts-fold-toggle
  "fc" 'ts-fold-close-all
  "fo" 'ts-fold-open-all

  "rb" 'revert-buffer
  "'"  'expand-abbrev
  "\/"  'unexpand-abbrev
  ;; LSP
  "lr"  'lsp-rename
  "lS"  'consult-lsp-symbols
  "ls"  'consult-imenu)

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

;; Use q to quit the read-only buffers
(defun db/evil-record-macro ()
  (interactive)
  (if buffer-read-only
      (quit-window)
    (call-interactively 'evil-record-macro)))

(defun db/evil-execute-last-macro ()
  (interactive)
  (evil-execute-macro 1 last-kbd-macro))

(with-eval-after-load 'evil-maps
  (define-key evil-normal-state-map (kbd "q") 'db/evil-record-macro)
  (define-key evil-normal-state-map (kbd "Q") 'db/evil-execute-last-macro))

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
