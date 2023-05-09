(use-package tree-sitter
  :hook (tree-sitter-after-on-hook . tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode)
  (add-to-list 'tree-sitter-major-mode-language-alist '(emacs-lisp-mode . elisp))
  (add-to-list 'tree-sitter-major-mode-language-alist '(clojure-mode . clojure)))

(use-package tree-sitter-langs)

(use-package ts-fold
  :after tree-sitter
  :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
  :config
  (global-ts-fold-mode))

(use-package ts-fold-indicators
  :straight (ts-fold-indicators :type git :host github :repo "emacs-tree-sitter/ts-fold"))

;; (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(setq treesit-extra-load-path '("/home/dbohomiakov/work/tree-sitter-module/dist/"))
