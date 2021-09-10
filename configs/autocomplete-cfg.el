;; Company autocompletion
;; (use-package
;;   company
;;   :after lsp-mode
;;   :hook (lsp-mode . company-mode)
;;   :custom (company-minimum-prefix-length 3)
;;   (company-idle-delay 0.1))
;; (add-hook 'after-init-hook 'global-company-mode)

;; (use-package
;;   company-box
;;   :hook (company-mode . company-box-mode))

;; Autocompletion
;; (use-package
;;   counsel
;;   :bind (("C-c f" . counsel-projectile-find-file)
;;          ("C-c C-r" . counsel-rg)
;;          ("C-c r" . counsel-projectile-rg))
;;   :init (counsel-mode 1)
;;   :custom (counsel-async-command-delay 0.5))

;; (use-package
;;   orderless)

;; (use-package
;;   ivy

;;   :diminish
;;   :bind (:map ivy-minibuffer-map
;;               ("TAB" . ivy-alt-done)
;;               ("C-l" . ivy-alt-done)
;;               ("C-j" . ivy-next-line)
;;               ("C-k" . ivy-previous-line)
;;               :map ivy-switch-buffer-map
;;               ("C-k" . ivy-previous-line)
;;               ("C-l" . ivy-done)
;;               ("C-d" . ivy-switch-buffer-kill)
;;               :map ivy-reverse-i-search-map
;;               ("C-k" . ivy-previous-line)
;;               ("C-d" . ivy-reverse-i-search-kill))
;;   :init (ivy-mode 1)
;;   :config (setq ivy-use-virtual-buffers t)
;;   (setq ivy-wrap t)
;;   (setq ivy-initial-inputs-alist nil)
;;   (setq ivy-count-format "(%d/%d) ")
;;   (setq enable-recursive-minibuffers t)
;;   (setq ivy-use-selectable-prompt t)
;;   (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)
;;                                 (counsel-rg . ivy--regex)
;;                                 (counsel-grep-or-swiper . ivy--regex)
;;                                 ;; (counsel-M-x . ivy--regex-fuzzy)
;;                                 ;; (counsel-buffer-or-recentf . ivy--regex-ignore-order)
;;                                 ;; (counsel-find-file . ivy--regex-fuzzy)
;;                                 (t . ivy--regex-ignore-order))))

;; (use-package
;;   ivy-hydra)

;; ;; (use-package
;; ;;   ivy-posframe
;; ;;   :after ivy
;; ;;   :diminish
;; ;;   :config (setq ivy-posframe-display-functions-alist '((swiper .
;; ;;                                                                ivy-posframe-display-at-window-center)
;; ;;                                                        (t .
;; ;;                                                           ivy-posframe-display))
;; ;;                 ivy-posframe-height-alist '((t . 10)) ivy-posframe-parameters
;; ;;                 '((internal-border-width . 10)
;; ;;                   (left-fringe . 8)
;; ;;                   (right-fringe . 8)))
;; ;;   (setq ivy-posframe-width 70)
;; ;;   (ivy-posframe-mode +1))

;; ;; Improves sorting for fuzzy-matched results
;; (use-package
;;   flx
;;   :defer t
;;   :after ivy
;;   :init (setq ivy-flx-limit 10000))

;; ;; Adds M-x recent command sorting for counsel-M-x
;; (use-package
;;   smex
;;   :defer 1
;;   :after counsel)

;; (use-package
;;   ivy-rich
;;   :init (ivy-rich-mode 1))

;; (use-package
;;   all-the-icons-ivy-rich
;;   :ensure t
;;   :init (all-the-icons-ivy-rich-mode 1))

(use-package
  rg)

(use-package
  selectrum
  :config (selectrum-mode +1))

(use-package
  selectrum-prescient
  :after prescient
  :config
  ;; to make sorting and filtering more intelligent
  (selectrum-prescient-mode +1)
  ;; to save your command history on disk, so the sorting gets more
  ;; intelligent over time
  (setq selectrum-prescient-enable-filtering nil)
  (prescient-persist-mode +1))

(use-package
  company-prescient
  :after prescient
  :config
  ;; to make sorting and filtering more intelligent
  (company-prescient-mode +1)
  ;; to save your command history on disk, so the sorting gets more
  ;; intelligent over time
  (prescient-persist-mode +1))

;; Example configuration for Consult
(use-package
  consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)     ;; orig. yank-pop
         ("<help> a" . consult-apropos) ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)   ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         :map isearch-mode-map ("M-e" . consult-isearch) ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch) ;; orig. isearch-edit-string
         ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)) ;; needed by consult-line to detect isearch

  ;; Enable automatic preview at point in the *Completions* buffer.
  ;; This is relevant when you use the default completion UI,
  ;; and not necessary for Vertico, Selectrum, etc.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0 register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview
              :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple
              :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize consult-theme
                     :preview-key '(:debounce 0.2
                                              any)
                     consult-ripgrep consult-git-grep consult-grep consult-bookmark
                     consult-recent-file consult-xref consult--source-file
                     consult--source-project-file consult--source-bookmark
                     :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
;;;; 1. project.el (project-roots)
  (setq consult-project-root-function (lambda ()
                                        (when-let (project (project-current))
                                          (car (project-roots project)))))
;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
  )

(use-package
  corfu
  :bind (:map corfu-map
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous)
              ("C-f" . corfu-insert))
  :custom (corfu-cycle t)
  (corfu-auto t)
  (corfu-echo-documentation t)
  ;; (corfu-quit-at-boundary t)
  :init (corfu-global-mode))

;; Dabbrev works with Corfu
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))
