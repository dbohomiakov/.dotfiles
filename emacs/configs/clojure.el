(use-package flycheck-clj-kondo)

(use-package flycheck-joker)

(use-package clojure-mode
  :config
  (require 'flycheck-clj-kondo)
  (dolist
      (checkers
       '
       ((clj-kondo-clj . clojure-joker)
        (clj-kondo-cljs . clojurescript-joker)
        (clj-kondo-cljc . clojure-joker)
        (clj-kondo-edn . edn-joker)))
    (flycheck-add-next-checker
     (car checkers)
     (cons 'error (cdr checkers))))
  )

(use-package cider
  :hook
  ;; Always use in repl insert mode
  (cider-repl-mode-hook . evil-insert-state))

(add-hook 'emacs-lisp-mode 'hs-minor-mode)

(use-package lispy)

(use-package symex
  :disabled
  :after evil
  :config (setq symex--user-evil-keyspec
                '(("j" . symex-go-up)
                  ("k" . symex-go-down)
                  ("C-j" . symex-climb-branch)
                  ("C-k" . symex-descend-branch)
                  ("M-j" . symex-goto-highest)
                  ("M-k" . symex-goto-lowest)))
  (symex-initialize)
  (global-set-key
   (kbd "s-\\")
   'symex-mode-interface)
  :custom (symex-modal-backend 'evil))
