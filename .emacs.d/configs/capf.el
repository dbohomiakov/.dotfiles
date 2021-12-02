;; Company autocompletion
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.1))

(add-hook 'after-init-hook 'global-company-mode)

(use-package company-prescient
  :after prescient
  :init (company-prescient-mode +1))

;; Abbreviation completition
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

;; Corfu autocompletition
(use-package corfu
  ;; can be enabled instead company after resolving issues
  ;; with lsp-mode https://github.com/emacs-lsp/lsp-mode/pull/2975
  :disabled
  :after evil
  :bind (:map corfu-map
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous)
              ("C-f" . corfu-insert))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-commit-predicate t)
  (corfu-echo-documentation t)
  (corfu-quit-at-boundary t)
  :init (corfu-global-mode))
