(defun db/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a word"
  (interactive "p")
  (if
    minibuffer-completing-file-name
    ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
    (if (string-match-p "/." (minibuffer-contents))
      (zap-up-to-char (- arg) ?/)
      (delete-minibuffer-contents))
    (backward-kill-word arg)))

(use-package vertico
  :bind
  (:map
    vertico-map
    ("C-j" . vertico-next)
    ("C-k" . vertico-previous)
    ("C-f" . vertico-exit)
    :map
    minibuffer-local-map
    ("M-h" . db/minibuffer-backward-kill))
  :custom (vertico-cycle t)
  :init (vertico-mode))

;; Add funcy prefix before candidates
(advice-add #'vertico--format-candidate
  :around
  (lambda (orig cand prefix suffix index _start)
    (setq cand (funcall orig cand prefix suffix index _start))
    (concat
      (if (= vertico--index index)
        (propertize "» " 'face 'vertico-current)
        "  ")
      cand)))

(use-package orderless
  :config
  ;; Recognizes the following patterns:
  ;; * ~flex flex~
  ;; * =literal literal=
  ;; * %char-fold char-fold%
  ;; * `initialism initialism`
  ;; * !without-literal without-literal!
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun my-orderless-dispatch (pattern _index _total)
    (
      cond
      ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
      ((string-suffix-p "$" pattern)
        `
        (orderless-regexp
          .
          ,(concat (substring pattern 0 -1) "[\x100000-\x10FFFD]*$")))
      ;; File extensions
      ((string-match-p "\\`\\.." pattern)
        `
        (orderless-regexp
          .
          ,
          (concat
            "\\."
            (substring pattern 1)
            "[\x100000-\x10FFFD]*$")))
      ;; Ignore single !
      ((string= "!" pattern)
        `(orderless-literal . ""))
      ;; Character folding
      ((string-prefix-p "%" pattern)
        `(char-fold-to-regexp . ,(substring pattern 1)))
      ((string-suffix-p "%" pattern)
        `(char-fold-to-regexp . ,(substring pattern 0 -1)))
      ;; Without literal
      ((string-prefix-p "!" pattern)
        `(orderless-without-literal . ,(substring pattern 1)))
      ((string-suffix-p "!" pattern)
        `(orderless-without-literal . ,(substring pattern 0 -1)))
      ;; Initialism matching
      ((string-prefix-p "`" pattern)
        `(orderless-initialism . ,(substring pattern 1)))
      ((string-suffix-p "`" pattern)
        `(orderless-initialism . ,(substring pattern 0 -1)))
      ;; Literal matching
      ((string-prefix-p "=" pattern)
        `(orderless-literal . ,(substring pattern 1)))
      ((string-suffix-p "=" pattern)
        `(orderless-literal . ,(substring pattern 0 -1)))
      ;; Flex matching
      ((string-prefix-p "~" pattern)
        `(orderless-flex . ,(substring pattern 1)))
      ((string-suffix-p "~" pattern)
        `(orderless-flex . ,(substring pattern 0 -1)))))
  (setq
    completion-styles
    '(orderless)
    completion-category-defaults
    nil
    ;;; Enable partial-completion for files.
    ;;; Either give orderless precedence or partial-completion.
    ;;; Note that completion-category-overrides is not really an override,
    ;;; but rather prepended to the default completion-styles.
    completion-category-overrides
    '((file (styles orderless partial-completion))) ;; orderless is tried first
    ;; completion-category-overrides '((file (styles partial-completion))) ;; partial-completion is tried first
    orderless-component-separator
    #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
    orderless-style-dispatchers
    '(my-orderless-dispatch)))

;; Persist history over Emacs restarts
(use-package savehist
  :config
  (setq history-length 25)
  (savehist-mode 1))

(use-package prescient
  :config
  (setq selectrum-refine-candidates-function #'orderless-filter)
  (setq selectrum-highlight-candidates-function
    #'orderless-highlight-matches)
  ;; to save your command history on disk, so the sorting gets more
  ;; intelligent over time
  (prescient-persist-mode +1))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind
  ( ;; C-c bindings (mode-specific-map)
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
    ("M-y" . consult-yank-pop) ;; orig. yank-pop
    ("<help> a" . consult-apropos) ;; orig. apropos-command
    ;; M-g bindings (goto-map)
    ("M-g e" . consult-compile-error)
    ("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
    ("M-g g" . consult-goto-line) ;; orig. goto-line
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
    :map
    isearch-mode-map
    ("M-e" . consult-isearch) ;; orig. isearch-edit-string
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
  (setq
    register-preview-delay
    0
    register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple
    :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq
    xref-show-xrefs-function
    #'consult-xref
    xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
    consult-theme
    :preview-key
    '(:debounce 0.2 any)
    consult-ripgrep
    consult-git-grep
    consult-grep
    consult-bookmark
    consult-recent-file
    consult-xref
    consult--source-file
    consult--source-project-file
    consult--source-bookmark
    :preview-key (kbd "M-."))

  (setq consult-narrow-key "<") ;; (kbd "C-+")
  (setq consult-project-root-function #'projectile-project-root)
  :custom
  (consult-preview-excluded-hooks
    '
    (python-mode
      epa-file-find-file-hook
      recentf-track-opened-file
      vc-refresh-state)))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators
    '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init (marginalia-mode))

(use-package embark
  :after marginalia
  :ensure t
  :bind
  (("C-." . embark-act) ;; pick some comfortable binding
    ("C-;" . embark-dwim) ;; good alternative: M-.
    ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
    '
    ("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
      nil
      (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package ripgrep)
(use-package ag)

;;;;;;;;;;; SELECTRUM ;;;;;;;;;;;;;;;;;;
(use-package selectrum
  :disabled
  :bind
  (:map
    selectrum-minibuffer-map
    ("C-j" . selectrum-next-candidate)
    ("C-k" . selectrum-previous-candidate))
  :init (selectrum-mode +1)
  :config
  ;; Optional performance optimization
  (setq orderless-skip-highlighting (lambda () selectrum-is-active))
  (setq selectrum-highlight-candidates-function
    #'orderless-highlight-matches))

(use-package selectrum-prescient
  :disabled
  :after prescient
  :config
  ;; to make sorting and filtering more intelligent
  (selectrum-prescient-mode +1)
  (setq selectrum-prescient-enable-filtering nil))

;;;;;;;;;;; IVY ;;;;;;;;;;;;;;
(use-package counsel
  :disabled
  :init (counsel-mode 1)
  :custom (counsel-async-command-delay 0.5))

(use-package ivy
  :disabled
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

(use-package ivy-hydra
  :disabled)

(use-package ivy-posframe
  :disabled
  :after ivy
  :diminish
  :config (setq ivy-posframe-display-functions-alist '((swiper .
                                                               ivy-posframe-display-at-window-center)
                                                       (t .
                                                          ivy-posframe-display))
                ivy-posframe-height-alist '((t . 10)) ivy-posframe-parameters
                '((internal-border-width . 10)
                  (left-fringe . 8)
                  (right-fringe . 8)))
  (setq ivy-posframe-width 70)
  (ivy-posframe-mode +1))

;; Improves sorting for fuzzy-matched results
(use-package
  flx
  :defer t
  :after ivy
  :init (setq ivy-flx-limit 10000))

;; Adds M-x recent command sorting for counsel-M-x
(use-package smex
  :disabled
  :defer 1
  :after counsel)

(use-package ivy-rich
  :disabled
  :init (ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
  :disabled
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))
