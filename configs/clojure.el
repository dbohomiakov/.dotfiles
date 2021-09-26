(use-package flycheck-clj-kondo)

(use-package flycheck-joker)

(use-package clojure-mode
  :config (require 'flycheck-clj-kondo))

(use-package cider)
;; Always use in repl insert mode
(add-hook 'cider-repl-mode-hook 'evil-insert-state)

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

;; (use-package elisp-autofmt
;;   :commands (elisp-autofmt-save-hook-for-this-buffer)
;;   :hook
;;   (emacs-lisp-mode
;;     .
;;     (apply-partially 'elisp-autofmt-save-hook-for-this-buffer t))

;;   :straight
;;   (elisp-autofmt
;;     :type git
;;     :host gitlab
;;     :files (:defaults "elisp-autofmt")
;;     :repo "ideasman42/emacs-elisp-autofmt"))

(add-hook 'emacs-lisp-mode 'hs-minor-mode)
