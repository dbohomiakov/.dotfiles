;; (use-package
;;  esh-autosuggest
;;  :hook (eshell-mode . esh-autosuggest-mode))


;; (defun project-eshell-other-window ()
;;   "Open a `shell' in a new window."
;;   (interactive)
;;   (let ((buf (project-eshell)))
;;     (switch-to-buffer (other-buffer buf))
;;     (switch-to-buffer-other-window buf)))


;; (defun project-async-shell-command-other-window ()
;;   "Open a `shell' in a new window."
;;   (interactive)
;;   (let ((buf (project-async-shell-command)))
;;     (switch-to-buffer (other-buffer buf))
;;     (switch-to-buffer-other-window buf)))

;; (require 'fish-completion)

;; (use-package shell-pop)
;:custom
;(shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell))))

(use-package
 vterm
 :custom
 (vterm-shell "fish")
 (vterm-clear-scrollback-when-clearing t)
 :config
 (setq vterm-kill-buffer-on-exit t)
 (setq vterm-copy-exclude-prompt t)
 (setq vterm-ignore-blink-cursor nil)
 (advice-add
  #'vterm--redraw
  :after (lambda (&rest args) (evil-refresh-cursor evil-state))))

(use-package
 vterm-toggle
 :ensure t
 :custom
 (vterm-toggle-scope 'project)
 (vterm-toggle-hide-method 'reset-window-configration)
 :hook (vterm-toggle-show . evil-insert-state))


;; (setq vterm-toggle-fullscreen-p t)
;; (add-to-list
;;  'display-buffer-alist
;;  '((lambda (bufname _)
;;      (with-current-buffer bufname
;;        (equal major-mode 'vterm-mode)))
;;    (display-buffer-reuse-window display-buffer-same-window)))


(setq vterm-toggle-fullscreen-p nil)
(add-to-list
 'display-buffer-alist
 '((lambda (buffer-or-name _)
     (let ((buffer (get-buffer buffer-or-name)))
       (with-current-buffer buffer
         (or (equal major-mode 'vterm-mode)
             (string-prefix-p
              vterm-buffer-name (buffer-name buffer))))))
   (display-buffer-reuse-window display-buffer-in-side-window)
   (side . bottom)
   (dedicated . t) ;dedicated is supported in emacs27
   (reusable-frames . visible)
   (window-height . 0.3)))


(global-set-key [f2] 'vterm-toggle)
(global-set-key [C-f2] 'vterm-toggle-cd)

(defun evil-collection-vterm-escape-stay ()
  "Go back to normal state but don't move
cursor backwards. Moving cursor backwards is the default vim behavior but it is
not appropriate in some cases like terminals."
  (setq-local evil-move-cursor-back nil))

(add-hook 'vterm-mode-hook #'evil-collection-vterm-escape-stay)

(define-key
 vterm-mode-map (kbd "<C-backspace>")
 (lambda ()
   (interactive)
   (vterm-send-key (kbd "C-w"))))
