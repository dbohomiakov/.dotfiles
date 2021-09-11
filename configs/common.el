;; Eval elisp functions
(global-set-key (kbd "M-:") 'pp-eval-expression)

;; General
(setq inhibit-startup-screen t)

;; Cursor settings
(set-cursor-color "#f6f6f6")
(blink-cursor-mode -1)                  ; Disable blinking cursor

(scroll-bar-mode -1)                    ; Disable visible scrollbar
(tool-bar-mode -1)                      ; Disable the toolbar
(tooltip-mode -1)                       ; Disable tooltip
(set-fringe-mode 10)                    ; Give some brithing room
(menu-bar-mode -1)                      ; Disable menu bar
(setq visible-bell nil)                 ; Enable visible bell

;; nice scrolling
(setq scroll-margin 0 scroll-preserve-screen-position 1)

(global-display-line-numbers-mode)     ; enable showing lines numbers
(setq column-number-mode t)            ; enable showing column numbers
(setq size-indication-mode t)
(setq require-final-newline t)  ; Newline at end of file
(delete-selection-mode t)       ; Delete the selection with a keypress
(global-auto-revert-mode t) ; revert buffers automatically when underlying files are changed externally

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Enable case-switching
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq tab-always-indent 'complete) ;; smart tab behavior - indent or complete

;; Ask 'y' or 'n' for closing buffer
(fset 'yes-or-no-p 'y-or-n-p)

(use-package
  better-defaults)

;; Commenter
(use-package
  evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

;; optional if you want which-key integration
(use-package
  which-key
  :config (which-key-mode))

;; Use mode for debugging command and keys entered
(use-package
  command-log-mode)

(use-package
  tramp)

;; Higlight braces
(use-package
  paren
  :config (set-face-attribute 'show-paren-match-expression nil
                              :background "#363e4a")
  (show-paren-mode 1))

(use-package
  smartparens
  :config (smartparens-global-mode 1))

(use-package
  yasnippet)

(use-package
  json-mode)

(use-package
  highlight-indent-guides
  :custom (highlight-indent-guides-method 'bitmap)
  :hook (emacs-lisp-mode . highlight-indent-guides-mode))

(use-package
  markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "/usr/bin/pandoc"))
