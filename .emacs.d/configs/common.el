;; Fix for long lines hangs https://www.gnu.org/software/emacs/manual/html_node/emacs/Long-Lines.html
(global-so-long-mode 1)
;; took from http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; Eval elisp functions
(global-set-key (kbd "M-:") 'pp-eval-expression)

;; General
(setq inhibit-startup-screen t)

;; Cursor settings
(set-cursor-color "#f6f6f6")
(setq visible-cursor nil) ; Disable blinking cursor for terminal
(blink-cursor-mode -1) ; Disable blinking cursor for XWindow

(scroll-bar-mode -1) ; Disable visible scrollbar
(horizontal-scroll-bar-mode -1) ; Disable visible horizontal scrollbar
(tool-bar-mode -1) ; Disable the toolbar
(tooltip-mode -1) ; Disable tooltip
(set-fringe-mode 10) ; Give some brithing room
(menu-bar-mode -1) ; Disable menu bar
(setq visible-bell nil) ; Enable visible bell

;; nice scrolling
(setq
  scroll-margin
  0
  scroll-preserve-screen-position 1)

;; Enable showing lines numbers in relative style
(when (display-graphic-p)
    (setq-default display-line-numbers-type 'relative
                  display-line-numbers-current-absolute t
                  display-line-numbers-width 4
                  display-line-numbers-widen t))
(global-display-line-numbers-mode)

(setq column-number-mode t) ; enable showing column numbers
(setq require-final-newline t) ; Newline at end of file
(delete-selection-mode t) ; Delete the selection with a keypress
(global-auto-revert-mode t) ; revert buffers automatically when underlying files are changed externally

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Enable case-switching
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Make kill-ring work with X-clipboard
(setq select-enable-primary t)

(setq tab-always-indent 'complete) ;; smart tab behavior - indent or complete
(setq-default indent-tabs-mode nil)

;; Ask 'y' or 'n' for closing buffer
(fset 'yes-or-no-p 'y-or-n-p)

(require 'uniquify)
;; construct unique buffer names for files with the same base name
(setq uniquify-buffer-name-style 'forward)

;; https://www.emacswiki.org/emacs/SavePlace
(save-place-mode 1)
(show-paren-mode 1)

(setq
  save-interprogram-paste-before-kill
  t
  apropos-do-all t
  mouse-yank-at-point t
  require-final-newline t
  load-prefer-newer t
  ediff-window-setup-function 'ediff-setup-windows-plain)

;; Commenter
(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

;; optional if you want which-key integration
(use-package which-key
  :config (which-key-mode))

;; Use mode for debugging command and keys entered
(use-package command-log-mode
  :defer)

(use-package tramp
  :defer)

;; Higlight braces
(use-package paren
  :config
  (set-face-attribute
    'show-paren-match-expression
    nil
    :background "#363e4a")
  (show-paren-mode 1))

(use-package smartparens
  :config
  (smartparens-global-mode 1)
  ;; Setup to not insert double parens before text object, only single
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'clojure-mode "'" nil :actions nil))

(use-package yasnippet)

(use-package highlight-indent-guides
  :custom (highlight-indent-guides-method 'bitmap)
  :hook (emacs-lisp-mode . highlight-indent-guides-mode))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
    ("\\.md\\'" . markdown-mode)
    ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "/usr/bin/pandoc"))

;; Do not ask about unsafe dir-locals
(advice-add 'risky-local-variable-p :override #'ignore)

(use-package flymake-json)
(use-package json-mode
  :mode
  (("\\.json\\'" . json-mode))
  :hook
  (json-mode . flycheck-mode)
  (json-mode . flymake-json-load))

(use-package yaml-mode
  :mode
  (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)))

(use-package toml-mode
  :mode
  (("\\.toml\\'" . toml-mode)))

(use-package csv-mode
  :mode
  (("\\.[Cc][Ss][Vv]\\'" . csv-mode))
  :custom
  (csv-separators '("," "\t" "|" " ")))
