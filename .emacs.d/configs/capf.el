;; Company autocompletion
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :custom
  (company-minimum-prefix-length 2)
  (company-tooltip-align-annotations t)
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
  :init
  (corfu-global-mode))

(use-package cape
  :disabled
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

(use-package kind-icon
  :disabled
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package corfu-doc
  :disabled
  :after corfu
  :straight (:host github :repo "galeo/corfu-doc")
  :init
  (define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down) ;; corfu-next
  (define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up)  ;; corfu-previous
  (define-key corfu-map (kbd "M-d") #'corfu-doc-toggle))

(add-hook 'corfu-mode-hook #'corfu-doc-mode)

(use-package all-the-icons-completion
  :init
  (all-the-icons-completion-mode))

(add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)

(use-package evil-goggles
  :after evil
  :ensure t
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces)
  (setq evil-goggles-pulse t))

(custom-set-faces
 '(evil-goggles-delete-face ((t (:inherit 'shadow))))
 '(evil-goggles-paste-face ((t (:inherit 'lazy-highlight))))
 '(evil-goggles-yank-face ((t (:inherit 'isearch-fail)))))
