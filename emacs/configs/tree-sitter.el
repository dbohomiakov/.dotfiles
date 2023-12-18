(use-package
 tree-sitter
 :hook (tree-sitter-after-on-hook . tree-sitter-hl-mode)
 :config (global-tree-sitter-mode)
 (setq
  tree-sitter-debug-jump-buttons t
  ;; and this highlights the entire sub tree in your code
  tree-sitter-debug-highlight-jump-region t)
 (add-to-list
  'tree-sitter-major-mode-language-alist '(emacs-lisp-mode . elisp))
 (add-to-list
  'tree-sitter-major-mode-language-alist '(clojure-mode . clojure)))

(use-package tree-sitter-langs)

(use-package
 ts-fold
 :after tree-sitter
 :straight
 (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
 :config (global-ts-fold-mode))

(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(use-package
 treesitter-context
 :straight
 :disabled
 (:host github :type git :repo "zbelial/treesitter-context.el")
 :init
 (use-package
  posframe-plus
  :straight
  (:host github :type git :repo "zbelial/posframe-plus")
  :config (treesitter-context-mode t)))

(use-package posframe)

; ;; BUILTIN TREESIT
;; (use-package
;;  treesit-auto
;;  :custom (treesit-auto-install 'prompt)
;;  :config
;;  (treesit-auto-add-to-auto-mode-alist 'all)
;;  (global-treesit-auto-mode))

; (use-package
;  evil-ts
;  :disabled
;  :straight
;  (evil-ts :type git :host github :repo "foxfriday/evil-ts"))
