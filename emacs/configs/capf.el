;; Company autocompletion
(use-package
 company
 ;; :disabled
 :custom
 ;; (company-minimum-prefix-length 2)
 ;; (company-tooltip-align-annotations t)
 ;; (company-idle-delay 0.1)
 ;; disable company mode
 (company-global-modes nil))

;; Abbreviation completition
(use-package
 dabbrev
 ;; Swap M-/ and C-M-/
 :bind (("M-/" . dabbrev-completion) ("C-M-/" . dabbrev-expand)))

(use-package
 all-the-icons-completion
 :init (all-the-icons-completion-mode))

(add-hook
 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)

(use-package
 evil-goggles
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

(defun bb-company-capf (f &rest args)
  "Manage `completion-styles'."
  (if (length< company-prefix 2)
      (let ((completion-styles (remq 'fussy completion-styles)))
        (apply f args))
    (apply f args)))

(defun bb-company-transformers (f &rest args)
  "Manage `company-transformers'."
  (if (length< company-prefix 2)
      (apply f args)
    (let ((company-transformers
           '(fussy-company-sort-by-completion-score)))
      (apply f args))))

(advice-add
 'company--transform-candidates
 :around 'bb-company-transformers)
(advice-add 'company-capf :around 'bb-company-capf)

(use-package
 corfu
 :load-path "straight/build/corfu/extensions/"
 :after evil
 :hook (lsp-completion-mode . kb/corfu-setup-lsp) ; Use corfu for lsp completion
 :bind
 (:map
  corfu-map
  ("C-j" . corfu-next)
  ("C-k" . corfu-previous)
  ("C-f" . corfu-insert)
  ("H-SPC" . corfu-insert-separator)
  ("M-d" . corfu-show-documentation)
  ("C-g" . corfu-quit)
  ("M-l" . corfu-show-location))
 :custom
 ;; Works with `indent-for-tab-command'. Make sure tab doesn't indent when you
 ;; want to perform completion
 (tab-always-indent 'complete)
 (completion-cycle-threshold nil) ; Always show candidates in menu

 (corfu-auto t)
 (corfu-auto-prefix 2)
 (corfu-auto-delay 0.3)

 ;; (corfu-min-width 80)
 ;; (corfu-max-width corfu-min-width)     ; Always have the same width
 (corfu-count 14)
 (corfu-scroll-margin 4)
 (corfu-cycle nil)

 ;; `nil' means to ignore `corfu-separator' behavior, that is, use the older
 ;; `corfu-quit-at-boundary' = nil behavior. Set this to separator if using
 ;; `corfu-auto' = `t' workflow (in that case, make sure you also set up
 ;; `corfu-separator' and a keybind for `corfu-insert-separator', which my
 ;; configuration already has pre-prepared). Necessary for manual corfu usage with
 ;; orderless, otherwise first component is ignored, unless `corfu-separator'
 ;; is inserted.
 (corfu-quit-at-boundary nil)
 (corfu-separator ?\s) ; Use space
 (corfu-quit-no-match 'separator) ; Don't quit if there is `corfu-separator' inserted
 (corfu-preview-current 'insert) ; Preview first candidate. Insert on input if only one
 (corfu-preselect-first t) ; Preselect first candidate?

 ;; Other
 (corfu-echo-documentation nil) ; Already use corfu-doc
 (lsp-completion-provider :none) ; Use corfu instead for lsp completions
 (corfu-popupinfo-max-width 70)
 (corfu-popupinfo-min-width 20)
 :init
 (global-corfu-mode)
 (require 'corfu-popupinfo)
 (corfu-popupinfo-mode 1)
 :config
 ;; NOTE 2022-03-01: This allows for a more evil-esque way to have
 ;; `corfu-insert-separator' work with space in insert mode without resorting to
 ;; overriding keybindings with `general-override-mode-map'. See
 ;; https://github.com/minad/corfu/issues/12#issuecomment-869037519
 ;; Alternatively, add advice without `general.el':
 (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
 (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
 ;; (general-add-advice '(corfu--setup corfu--teardown) :after 'evil-normalize-keymaps)
 ;; (evil-make-overriding-map corfu-map)

 ;; Enable Corfu more generally for every minibuffer, as long as no other
 ;; completion UI is active. If you use Mct or Vertico as your main minibuffer
 ;; completion UI. From
 ;; https://github.com/minad/corfu#completing-with-corfu-in-the-minibuffer
 (defun corfu-enable-always-in-minibuffer ()
   "Enable Corfu in the minibuffer if Vertico are not active."
   (unless (bound-and-true-p vertico--input)
     (setq-local corfu-auto nil) ; Ensure auto completion is disabled
     (corfu-mode 1)))
 (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer
           1)

 ;; Setup lsp to use corfu for lsp completion
 (defun kb/corfu-setup-lsp ()
   "Use orderless completion style with lsp-capf instead of the
default lsp-passthrough."
   (setf (alist-get
          'styles
          (alist-get 'lsp-capf completion-category-defaults))
         '(orderless))))

(use-package
 kind-icon
 :after corfu
 :custom
 (kind-icon-use-icons t)
 (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
 (kind-icon-blend-background nil) ; Use midpoint color between foreground and background colors ("blended")?
 (kind-icon-blend-frac 0.08)

 ;; NOTE 2022-02-05: `kind-icon' depends `svg-lib' which creates a cache
 ;; directory that defaults to the `user-emacs-directory'. Here, I change that
 ;; directory to a location appropriate to `no-littering' conventions, a
 ;; package which moves directories of other packages to sane locations.
 (svg-lib-icons-dir
  (no-littering-expand-var-file-name "svg-lib/cache/")) ; Change cache dir
 :config
 (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'

 ;; Add hook to reset cache so the icon colors match my theme
 ;; NOTE 2022-02-05: This is a hook which resets the cache whenever I switch
 ;; the theme using my custom defined command for switching themes. If I don't
 ;; do this, then the backgound color will remain the same, meaning it will not
 ;; match the background color corresponding to the current theme. Important
 ;; since I have a light theme and dark theme I switch between. This has no
 ;; function unless you use something similar
 (add-hook
  'kb/themes-hooks
  #'(lambda ()
      (interactive)
      (kind-icon-reset-cache))))

(use-package
 cape
 :demand t
 :hook
 ((emacs-lisp-mode . kb/cape-capf-setup-elisp)
  (lsp-completion-mode . kb/cape-capf-setup-lsp)
  ;; (org-mode . kb/cape-capf-setup-org)
  (eshell-mode . kb/cape-capf-setup-eshell)
  (magit-mode . kb/cape-capf-setup-git-commit)
  ;; (LaTeX-mode . kb/cape-capf-setup-latex)
  ;; (sh-mode . kb/cape-capf-setup-sh)
  )
 ;; :general (:prefix "M-c"               ; Particular completion function
 ;;                   "p" 'completion-at-point
 ;;                   "t" 'complete-tag   ; etags
 ;;                   "d" 'cape-dabbrev   ; or dabbrev-completion
 ;;                   "f" 'cape-file
 ;;                   "k" 'cape-keyword
 ;;                   "s" 'cape-symbol
 ;;                   "a" 'cape-abbrev
 ;;                   "i" 'cape-ispell
 ;;                   "l" 'cape-line
 ;;                   "w" 'cape-dict
 ;;                   "\\"' cape-tex
 ;;                   "_" 'cape-tex
 ;;                   "^" 'cape-tex
 ;;                   "&" 'cape-sgml
 ;;                   "r" 'cape-rfc1345
 ;;                   )
 :custom (cape-dabbrev-min-length 2)
 :init
 ;; Elisp
 (defun kb/cape-capf-ignore-keywords-elisp (cand)
   "Ignore keywords with forms that begin with \":\" (e.g. :history)."
   (or (not (keywordp cand))
       (eq (char-after (car completion-in-region--data)) ?:)))

 (defun kb/cape-capf-setup-elisp ()
   "Replace the default `elisp-completion-at-point'completion-at-point-function. Doing it this way will prevent disrupting the addition of other capfs (e.g. merely setting the variable entirely, or adding to list). Additionally, add `cape-file' as early as possible to the list."
   (setf (elt
          (cl-member
           'elisp-completion-at-point completion-at-point-functions)
          0)
         #'elisp-completion-at-point)
   (add-to-list 'completion-at-point-functions #'cape-symbol)
   ;; I prefer this being early/first in the list
   (add-to-list 'completion-at-point-functions #'cape-file)
   ;; (require 'company-yasnippet)
   ;; (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-yasnippet))
   )

 ;; Org
 ;; (defun kb/cape-capf-setup-org ()
 ;;   (require 'org-roam)
 ;;   (if (org-roam-file-p)
 ;;       (org-roam--register-completion-functions-h)
 ;;     (let (result
 ;;       (dolist (element (list
 ;;                         (cape-super-capf #'cape-ispell #'cape-dabbrev)
 ;;                         ;; (cape-company-to-capf #'company-yasnippet)
 ;;                         )
 ;;                        result)
 ;;         (add-to-list 'completion-at-point-functions element)))
 ;;     )))

 ;; Eshell
 (defun kb/cape-capf-setup-eshell ()
   (let ((result))
     (dolist (element
              '(pcomplete-completions-at-point cape-file) result)
       (add-to-list 'completion-at-point-functions element))))

 ;; Git-commit
 (defun kb/cape-capf-setup-git-commit ()
   (general-define-key
    :keymaps 'local
    :states 'insert
    "<tab>" 'completion-at-point) ; Keybinding for `completion-at-point'
   (let ((result))
     (dolist (element
              '(cape-symbol
                cape-dabbrev tags-completion-at-point-function)
              result)
       (add-to-list 'completion-at-point-functions element))))

 ;; LaTeX
 ;; (defun kb/cape-capf-setup-latex ()
 ;;   ;; (require 'company-auctex)
 ;;   (let ((result))
 ;;     (dolist (element (list
 ;;                       ;; First add `company-yasnippet'
 ;;                       ;; (cape-company-to-capf #'company-yasnippet)
 ;;                       ;; Then add `cape-tex'
 ;;                       #'cape-tex
 ;;                       ;; Then add `company-auctex' in the order it adds its
 ;;                       ;; backends.
 ;;                       (cape-company-to-capf #'company-auctex-bibs)
 ;;                       (cape-company-to-capf #'company-auctex-labels)
 ;;                       (cape-company-to-capf
 ;;                        (apply-partially #'company--multi-backend-adapter
 ;;                                         '(company-auctex-macros company-auctex-symbols company-auctex-environments))))
 ;;                      result)
 ;;       (add-to-list 'completion-at-point-functions element))))


 ;; Sh
 ;; (defun kb/cape-capf-setup-sh ()
 ;;   (require 'company-shell)
 ;;   (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-shell)))
 :config
 ;; For pcomplete. For now these two advices are strongly recommended to
 ;; achieve a sane Eshell experience. See
 ;; https://github.com/minad/corfu#completing-with-corfu-in-the-shell-or-eshell

 ;; Silence the pcomplete capf, no errors or messages!
 (advice-add
  'pcomplete-completions-at-point
  :around #'cape-wrap-silent)
 ;; Ensure that pcomplete does not write to the buffer and behaves as a pure
 ;; `completion-at-point-function'.
 (advice-add
  'pcomplete-completions-at-point
  :around #'cape-wrap-purify))


;; properly handle indent or completition
(setq tab-always-indent 'complete)


;; Configure Tempel
(use-package
 tempel
 ;; Require trigger prefix before template name when completing.
 ;; :custom
 ;; (tempel-trigger-prefix "<")

 :bind
 (("M-+" . tempel-complete) ;; Alternative tempel-expand
  ("M-*" . tempel-insert))

 :init

 ;; Setup completion at point
 (defun tempel-setup-capf ()
   ;; Add the Tempel Capf to `completion-at-point-functions'.
   ;; `tempel-expand' only triggers on exact matches. Alternatively use
   ;; `tempel-complete' if you want to see all matches, but then you
   ;; should also configure `tempel-trigger-prefix', such that Tempel
   ;; does not trigger too often when you don't expect it. NOTE: We add
   ;; `tempel-expand' *before* the main programming mode Capf, such
   ;; that it will be tried first.
   (setq-local completion-at-point-functions
               (cons #'tempel-expand completion-at-point-functions)))

 (add-hook 'prog-mode-hook 'tempel-setup-capf)
 (add-hook 'text-mode-hook 'tempel-setup-capf)

 ;; Optionally make the Tempel templates available to Abbrev,
 ;; either locally or globally. `expand-abbrev' is bound to C-x '.
 ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
 ;; (global-tempel-abbrev-mode)
 )

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package tempel-collection)
