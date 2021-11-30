(use-package flycheck-clj-kondo)

(use-package flycheck-joker)

(use-package clojure-mode
  :config (require 'flycheck-clj-kondo))

(use-package cider
  :hook
  ;; Always use in repl insert mode
  (cider-repl-mode-hook . evil-insert-state))

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

(add-hook 'emacs-lisp-mode 'hs-minor-mode)
