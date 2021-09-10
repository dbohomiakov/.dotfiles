;; Company autocompletion
(use-package
  company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :custom (company-minimum-prefix-length 3)
  (company-idle-delay 0.1))
(add-hook 'after-init-hook 'global-company-mode)

(use-package
  company-box
  :hook (company-mode . company-box-mode))

;; Autocompletion
(use-package
  counsel
  :bind (("C-c f" . counsel-projectile-find-file)
         ("C-c C-r" . counsel-rg)
         ("C-c r" . counsel-projectile-rg))
  :init (counsel-mode 1)
  :custom (counsel-async-command-delay 0.5))

(use-package
  orderless)

(use-package
  ivy

  :diminish
  :bind (:map ivy-minibuffer-map
              ("TAB" . ivy-alt-done)
              ("C-l" . ivy-alt-done)
              ("C-j" . ivy-next-line)
              ("C-k" . ivy-previous-line)
              :map ivy-switch-buffer-map
              ("C-k" . ivy-previous-line)
              ("C-l" . ivy-done)
              ("C-d" . ivy-switch-buffer-kill)
              :map ivy-reverse-i-search-map
              ("C-k" . ivy-previous-line)
              ("C-d" . ivy-reverse-i-search-kill))
  :init (ivy-mode 1)
  :config (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)
                                (counsel-rg . ivy--regex)
                                (counsel-grep-or-swiper . ivy--regex)
                                ;; (counsel-M-x . ivy--regex-fuzzy)
                                ;; (counsel-buffer-or-recentf . ivy--regex-ignore-order)
                                ;; (counsel-find-file . ivy--regex-fuzzy)
                                (t . ivy--regex-ignore-order))))

(use-package
  ivy-hydra)

;; (use-package
;;   ivy-posframe
;;   :after ivy
;;   :diminish
;;   :config (setq ivy-posframe-display-functions-alist '((swiper .
;;                                                                ivy-posframe-display-at-window-center)
;;                                                        (t .
;;                                                           ivy-posframe-display))
;;                 ivy-posframe-height-alist '((t . 10)) ivy-posframe-parameters
;;                 '((internal-border-width . 10)
;;                   (left-fringe . 8)
;;                   (right-fringe . 8)))
;;   (setq ivy-posframe-width 70)
;;   (ivy-posframe-mode +1))

;; Improves sorting for fuzzy-matched results
(use-package
  flx
  :defer t
  :after ivy
  :init (setq ivy-flx-limit 10000))

;; Adds M-x recent command sorting for counsel-M-x
(use-package
  smex
  :defer 1
  :after counsel)

(use-package
  ivy-rich
  :init (ivy-rich-mode 1))

(use-package
  all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package
  rg)
