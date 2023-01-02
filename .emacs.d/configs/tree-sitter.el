(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-to-list 'tree-sitter-major-mode-language-alist '(emacs-lisp-mode . elisp))
  (add-to-list 'tree-sitter-major-mode-language-alist '(clojure-mode . clojure)))

(use-package tree-sitter-langs)

(use-package ts-fold
  :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold"))

(use-package ts-fold-indicators
  :straight (ts-fold-indicators :type git :host github :repo "emacs-tree-sitter/ts-fold"))

;; (add-hook 'tree-sitter-after-on-hook #ts-fold-indicators-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(setq treesit-extra-load-path '("/home/dbohomiakov/work/tree-sitter-module/dist/"))
