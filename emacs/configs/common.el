;; Fix for long lines hangs https://www.gnu.org/software/emacs/manual/html_node/emacs/Long-Lines.html
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)

;; General
(setq inhibit-startup-screen t)

(scroll-bar-mode -1) ; Disable visible scrollbar
(horizontal-scroll-bar-mode -1) ; Disable visible horizontal scrollbar
(tool-bar-mode -1) ; Disable the toolbar
(tooltip-mode -1) ; Disable tooltip
(set-fringe-mode 10) ; Give some brithing room
(menu-bar-mode -1) ; Disable menu bar
(setq visible-bell nil) ; Enable visible bell
(setq ring-bell-function 'ignore)

;; Cursor settings
(set-cursor-color "#f6f6f6")
(setq visible-cursor nil) ; Disable blinking cursor for terminal
(blink-cursor-mode -1) ; Disable blinking cursor for XWindow
;; Make scrolling less stuttered
(setq auto-window-vscroll nil)
(customize-set-variable 'fast-but-imprecise-scrolling t)
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-margin 0)
(customize-set-variable 'scroll-preserve-screen-position t)
(pixel-scroll-precision-mode 1)

;; Enable showing lines numbers in relative style
(when (display-graphic-p)
  (setq-default
   display-line-numbers-type 'relative
   display-line-numbers-current-absolute t
   display-line-numbers-width 2
   display-line-numbers-grow-only t
   display-line-numbers-widen t))
;; (global-display-line-numbers-mode)
(setq column-number-mode t) ; enable showing column numbers
(setq require-final-newline t) ; Newline at end of file

(defun my-display-numbers-hook ()
  (display-line-numbers-mode 1))
(add-hook 'prog-mode-hook 'my-display-numbers-hook)

(delete-selection-mode t) ; Delete the selection with a keypress
(global-auto-revert-mode t) ; revert buffers automatically when underlying files are changed externally
(setq global-auto-revert-non-file-buffers t) ; Revert dired and other buffers

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
 save-interprogram-paste-before-kill t
 apropos-do-all t
 mouse-yank-at-point t
 require-final-newline t
 load-prefer-newer t
 ediff-window-setup-function 'ediff-setup-windows-plain)

;; Commenter
(use-package
 evil-nerd-commenter
 :bind ("M-;" . evilnc-comment-or-uncomment-lines))

;; optional if you want which-key integration
(use-package which-key :config (which-key-mode))

;; Use mode for debugging command and keys entered
(use-package command-log-mode :defer)

;; Higlight braces
(use-package
 paren
 :config
 (set-face-attribute 'show-paren-match-expression nil
                     :background "#363e4a")
 (show-paren-mode 1))

(use-package
 smartparens
 :config (smartparens-global-mode 1)
 ;; Setup to not insert double parens before text object, only single
 (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
 (sp-local-pair 'clojure-mode "'" nil :actions nil))

(use-package yasnippet :commands yas-minor-mode)

(use-package yasnippet-snippets)

(use-package
 highlight-indent-guides
 :custom (highlight-indent-guides-method 'bitmap)
 :hook (emacs-lisp-mode . highlight-indent-guides-mode))

(use-package
 markdown-mode
 :commands (markdown-mode gfm-mode)
 :mode
 (("README\\.md\\'" . gfm-mode)
  ("\\.md\\'" . markdown-mode)
  ("\\.markdown\\'" . markdown-mode))
 :init (setq markdown-command "/usr/bin/pandoc"))

;; Do not ask about unsafe dir-locals
(advice-add 'risky-local-variable-p :override #'ignore)

(use-package flymake-json)
(use-package
 json-mode
 :mode (("\\.json\\'" . json-mode))
 :hook
 (json-mode . flycheck-mode)
 (json-mode . flymake-json-load))

(use-package
 yaml-mode
 :init (setq yaml-indent-offset 2)
 :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)))

(use-package toml-mode :mode (("\\.toml\\'" . toml-mode)))

(use-package
 csv-mode
 :mode (("\\.[Cc][Ss][Vv]\\'" . csv-mode))
 :custom (csv-separators '("," "\t" "|" " ")))

(use-package markdown-mode :mode (("\\.md\\'" . markdown-mode)))

(use-package
 bazel
 :straight (:host github :repo "bazelbuild/emacs-bazel-mode")
 :mode (("\\.star\\'" . bazel-starlark-mode)))

(use-package xml-format :demand t :after nxml-mode)

(use-package
 select
 :demand t
 :custom
 (save-interprogram-paste-before-kill t)
 (select-enable-clipboard t)
 (selection-coding-system 'utf-8)
 :init (setq-default wl-copy-process nil)
 ;; check if terminal and window manager is wayland
 (when (and (not window-system)
            (string-prefix-p "wayland" (getenv "WAYLAND_DISPLAY")))
   (defun wl-copy-handler (text)
     (setq wl-copy-process
           (make-process
            :name "wl-copy"
            :buffer nil
            :command '("wl-copy" "-f" "-n")
            :connection-type 'pipe))
     (process-send-string wl-copy-process text)
     (process-send-eof wl-copy-process))
   (defun wl-paste-handler ()
     (if (and wl-copy-process (process-live-p wl-copy-process))
         nil ; should return nil if we're the current paste owner
       (shell-command-to-string "wl-paste -n | tr -d \r")))
   (setq
    interprogram-cut-function 'wl-copy-handler
    interprogram-paste-function 'wl-paste-handler)))

(use-package string-inflection)

(defun db/string-inflection-cycle-auto ()
  "switching by major-mode"
  (interactive)
  (cond
   ;; for emacs-lisp-mode
   ((eq major-mode 'emacs-lisp-mode)
    (string-inflection-all-cycle))
   ;; for python
   ((eq major-mode 'python-ts-mode)
    (string-inflection-python-style-cycle))
   ;; for java
   ((eq major-mode 'java-mode)
    (string-inflection-java-style-cycle))
   (t
    ;; default
    (string-inflection-ruby-style-cycle))))

;; Multi-language code formatting package
(use-package
 apheleia
 :init (apheleia-global-mode +1)
 :config
 (setf (alist-get 'python-ts-mode apheleia-mode-alist)
       '(isort black)))

(use-package
 elisp-autofmt
 :commands (elisp-autofmt-mode elisp-autofmt-buffer)
 :custom (elisp-autofmt-on-save-p 'always)
 :hook (emacs-lisp-mode . elisp-autofmt-mode))

(recentf-mode 1)

(add-to-list
 'recentf-exclude
 (recentf-expand-file-name no-littering-var-directory))
(add-to-list
 'recentf-exclude
 (recentf-expand-file-name no-littering-etc-directory))

(use-package
 ibuffer
 :ensure nil
 :preface
 (defvar protected-buffers '("*scratch*" "*Messages*")
   "Buffer that cannot be killed.")

 (use-package devdocs)

 (use-package
  xclip
  :disabled
  :config
  (setq xclip-program "wl-copy")
  (setq xclip-select-enable-clipboard t)
  (setq xclip-mode t)
  (setq xclip-method (quote wl-copy)))

 (defun my/protected-buffers ()
   "Protect some buffers from being killed."
   (dolist (buffer protected-buffers)
     (with-current-buffer buffer
       (emacs-lock-mode 'kill))))
 :init (my/protected-buffers))

(use-package
 visual-replace
 :defer t
 :straight (:host github :repo "szermatt/visual-replace"))

(use-package
 ultra-scroll
 :straight (:host github :repo "jdtsmith/ultra-scroll")
 :init
 (setq
  scroll-conservatively 101 ; important!
  scroll-margin 0)
 :config (ultra-scroll-mode 1))
