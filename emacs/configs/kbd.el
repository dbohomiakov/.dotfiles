;; format: off
(setq abbrev-file-name (concat db/emacs-dir "/abbrev_defs"))
;; Evil mode
;; Rebind Ctrl+g to escape
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; Rebind universal argument
(global-set-key (kbd "C-M-u") 'universal-argument)

;; use visual line motions even outside of visual-line-mode buffers
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

(use-package key-chord
             :demand t
             :after evil
             :custom
             (key-chord-two-keys-delay 0.1)
             (key-chord-safety-interval-forward 0.05)
             :init (key-chord-mode 1)
             (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
             (key-chord-define evil-visual-state-map "jk" 'evil-normal-state)
             (key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
             (key-chord-define evil-visual-state-map "kj" 'evil-normal-state))


(use-package general
             :demand t
             :after evil-collection
             :config (general-evil-setup t)
             (general-create-definer
              db/leader-keys
              :states '(normal insert visual emacs)
              :keymaps 'override
              :prefix "SPC"
              :global-prefix "C-SPC")

             (db/leader-keys
              :keymaps 'clojure-mode-map
              "ef" 'cider-eval-defun-at-point
              "er" 'cider-eval-region
              "eb" 'cider-eval-buffer)

             (db/leader-keys
              :keymaps 'emacs-lisp-mode-map
              "ef" 'eval-defun
              "er" 'eval-region
              "eb" 'eval-buffer)

             (db/leader-keys
              :keymaps 'python-ts-mode-map
              "ef" 'python-shell-send-defun
              "er" 'python-shell-send-region
              "eb" 'python-shell-send-buffer
              "es" 'python-shell-send-statement
              "ei" 'run-python)

             (db/leader-keys
              ;; Project
              "pf" 'project-find-file
              "ps" 'projectile-switch-project
              ;; "ps" 'consult-projectile-switch-project
              "pg" 'consult-ripgrep
              "pd" 'project-dired
              "pe" 'vterm-toggle-cd
              "pE" 'project-eshell
              "pC" 'project-compile
              "pp" 'consult-projectile
              "pb" 'consult-project-buffer
              "pc" 'project-async-shell-command
              "ff" 'find-file
              ;; Docker
              "tt" 'treemacs
              "ti" 'treemacs--button-close

              "rr" 'replace-regexp

              "n" 'evil-buffer-new

              "\\" 'evil-window-vsplit
              "-" 'evil-window-split
              "s" 'save-buffer
              "d" 'evil-window-delete
              "k" 'save-buffers-kill-terminal
              "b" 'consult-buffer

              ;; Git
              "gs" 'magit-status
              "gb" 'magit-blame
              "gd" 'magit-diff
              "gu" 'git-link
              "gc" 'magit-branch-checkout
              "gh" 'magit-log-buffer-file
              "gr" 'consult-gh-orgs
              "gl" 'magit-pull
              "gm" 'magit-commit
              "gp" 'magit-push
              ;; ORG
              "oe" 'org-ctrl-c-ctrl-c
              "or" 'org-babel-remove-result-one-or-many
              "oo" 'org-open-at-point
              "oa" 'org-agenda-goto
              "od" 'org-roam-dailies-find-today
              ;; KMACRO
              "mq" 'kmacro-start-macro
              "mQ" 'kmacro-end-macro
              "mn" 'kmacro-name-last-macro
              "me" 'kmacro-call-macro
              ;; Evil window
              "wn" 'evil-window-new
              "wo" 'delete-other-windows
              "ww" 'other-window
              "wd" 'ace-delete-window

              "rb" 'revert-buffer
              "'" 'expand-abbrev
              "\/" 'unexpand-abbrev
              ;; LSP
              "lr" 'eglot-rename
              "lS" 'consult-eglot-symbols
              "ls" 'consult-imenu
              "ld" 'consult-flymake
              "la" 'eglot-code-actions))

;; Use consult-line for incremental search
(evil-define-key 'normal 'global (kbd "/") #'consult-line)
(evil-define-key 'visual 'global (kbd "/") #'consult-line)
(evil-define-key 'normal 'global (kbd "?") #'avy-goto-char-2)
(evil-define-key 'normal 'visual (kbd "?") #'avy-goto-char-2)
(evil-define-key 'normal 'global (kbd "gs") #'evil-ex-sort)
;; Folding keys
(evil-define-key 'normal 'global (kbd "ghh") #'ts-fold-toggle)
(evil-define-key 'normal 'global (kbd "ghc") #'ts-fold-close-all)
(evil-define-key 'normal 'global (kbd "gho") #'ts-fold-open-all)
;; Multiple cursor (redefine default key sequences)
(evil-define-key
 '(normal visual) 'global (kbd "gm") evil-mc-cursors-map)
;; CamelCase/snake_case switch
(evil-define-key
 'normal 'global (kbd "gc") #'db/string-inflection-cycle-auto)
;; LSP
(evil-define-key 'normal 'global (kbd "gr") #'xref-find-references)
;; Yank kill ring
(evil-define-key 'normal 'global (kbd "gy") #'consult-yank-from-kill-ring)

(evil-define-key 'normal 'global (kbd "gb") #'evil-buffer)

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
                      (define-key
                       evil-normal-state-map (kbd "Q") 'db/evil-execute-last-macro))

;; Disable copy with selection
(fset 'evil-visual-update-x-selection 'ignore)

(use-package helpful
             :demand t
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
;; Eval elisp functions
(global-set-key (kbd "M-:") 'pp-eval-expression)
