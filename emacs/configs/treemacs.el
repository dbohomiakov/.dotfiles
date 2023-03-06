(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
  ;;         treemacs-deferred-git-apply-delay      0.5
  ;;         treemacs-directory-name-transformer    #'identity
  ;;         treemacs-display-in-side-window        t
  ;;         treemacs-eldoc-display                 t
  ;;         treemacs-file-event-delay              5000
  ;;         treemacs-file-extension-regex          treemacs-last-period-regex-value
  ;;         treemacs-file-follow-delay             0.2
  ;;         treemacs-file-name-transformer         #'identity
  ;;         treemacs-follow-after-init             t
  ;;         treemacs-git-command-pipe              ""
  ;;         treemacs-goto-tag-strategy             'refetch-index
  ;;         treemacs-indentation                   2
  ;;         treemacs-indentation-string            " "
  ;;         treemacs-is-never-other-window         nil
  ;;         treemacs-max-git-entries               5000
  ;;         treemacs-missing-project-action        'ask
  ;;         treemacs-move-forward-on-expand        nil
  ;;         treemacs-no-png-images                 nil
  ;;         treemacs-no-delete-other-windows       t
  ;;         treemacs-project-follow-cleanup        nil
  ;;         treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
  ;;         treemacs-position                      'left
  ;;         treemacs-read-string-input             'from-child-frame
  ;;         treemacs-recenter-distance             0.1
  ;;         treemacs-recenter-after-file-follow    nil
  ;;         treemacs-recenter-after-tag-follow     nil
  ;;         treemacs-recenter-after-project-jump   'always
  ;;         treemacs-recenter-after-project-expand 'on-distance
  ;;         treemacs-show-cursor                   nil
  ;;         treemacs-show-hidden-files             t
  ;;         treemacs-silent-filewatch              nil
  ;;         treemacs-silent-refresh                nil
  ;;         treemacs-sorting                       'alphabetic-asc
  ;;         treemacs-space-between-root-nodes      t
  ;;         treemacs-tag-follow-cleanup            t
  ;;         treemacs-tag-follow-delay              1.5
  ;;         treemacs-user-mode-line-format         nil
  ;;         treemacs-user-header-line-format       nil
  ;;         treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil
          treemacs-display-current-project-exclusively t)

  ;;   ;; The default width and height of the icons is 22 pixels. If you are
  ;;   ;; using a Hi-DPI display, uncomment this to double the icon size.
  ;;   ;; (treemacs-resize-icons 22)

    (treemacs-follow-mode t)
  ;;   (treemacs-filewatch-mode t)
  ;;   (treemacs-fringe-indicator-mode 'always)
  ;;   (pcase (cons (not (null (executable-find "git")))
  ;;                (not (null treemacs-python-executable)))
  ;;     (`(t . t)
  ;;      (treemacs-git-mode 'deferred))
  ;;     (`(t . _)
    ;;      (treemacs-git-mode 'simple)))
    )
  ;; :bind
  ;; (:map global-map
  ;;       ("M-0"       . treemacs-select-window)
  ;;       ("C-x t 1"   . treemacs-delete-other-windows)
  ;;       ("C-x t t"   . treemacs)
  ;;       ("C-x t B"   . treemacs-bookmark)
  ;;       ("C-x t C-t" . treemacs-find-file)
  ;;       ("C-x t M-t" . treemacs-find-tag))
  )

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after treemacs persp-mode ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

;; (use-package dirvish
;;   :init
;;   (dirvish-override-dired-mode)
;;   :custom
;;   (dirvish-quick-access-entries ; It's a :custom option
;;    '(("h" "~/"                          "Home")
;;      ("d" "~/Downloads/"                "Downloads")
;;      ("m" "/mnt/"                       "Drives")
;;      ("t" "~/.local/share/Trash/files/" "TrashCan")))
;;   :config
;;   ;; (dirvish-peek-mode) ; Preview files in minibuffer
;;   (setq dirvish-mode-line-format
;;         '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
;;   (setq dirvish-attributes
;;         '(all-the-icons file-size file-time collapse subtree-state vc-state))
;;   (setq delete-by-moving-to-trash t)
;;   (setq dired-listing-switches
;;         "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
;;   :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
;;   (("C-c f" . dirvish-fd)
;;    :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
;;    ("a"   . dirvish-quick-access)
;;    ("f"   . dirvish-file-info-menu)
;;    ("y"   . dirvish-yank-menu)
;;    ("N"   . dirvish-narrow)
;;    ("^"   . dirvish-history-last)
;;    ("h"   . dirvish-history-jump) ; remapped `describe-mode'
;;    ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
;;    ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
;;    ("TAB" . dirvish-subtree-toggle)
;;    ("M-f" . dirvish-history-go-forward)
;;    ("M-b" . dirvish-history-go-backward)
;;    ("M-l" . dirvish-ls-switches-menu)
;;    ("M-m" . dirvish-mark-menu)
;;    ("M-t" . dirvish-layout-toggle)
;;    ("M-s" . dirvish-setup-menu)
;;    ("M-e" . dirvish-emerge-menu)
;;    ("M-j" . dirvish-fd-jump)))

;; (dirvish-define-preview exa (file)
;;   "Use `exa' to generate directory preview."
;;   :require ("exa") ; tell Dirvish to check if we have the executable
;;   (when (file-directory-p file) ; we only interest in directories here
;;     `(shell . ("exa" "-al" "--color=always" "--icons"
;;                "--group-directories-first" ,file))))

;; (add-to-list 'dirvish-preview-dispatchers 'exa)
