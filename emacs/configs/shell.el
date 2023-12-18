(use-package
 esh-autosuggest
 :hook (eshell-mode . esh-autosuggest-mode))


(defun project-eshell-other-window ()
  "Open a `shell' in a new window."
  (interactive)
  (let ((buf (project-eshell)))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf)))


(defun project-async-shell-command-other-window ()
  "Open a `shell' in a new window."
  (interactive)
  (let ((buf (project-async-shell-command)))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf)))

(require 'fish-completion)

(use-package shell-pop)
;:custom
;(shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell))))

(use-package
 vterm
 :disabled
 :config
 (setq vterm-kill-buffer-on-exit t)
 (setq vterm-copy-exclude-prompt t)
 (setq vterm-ignore-blink-cursor nil)
 (advice-add
  #'vterm--redraw
  :after (lambda (&rest args) (evil-refresh-cursor evil-state))))

(use-package
 vterm-toggle
 :disabled
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
;; (global-set-key [f2] 'vterm-toggle)
;; (global-set-key [C-f2] 'vterm-toggle-cd)


;; (defun evil-collection-vterm-escape-stay ()
;;   "Go back to normal state but don't move
;; cursor backwards. Moving cursor backwards is the default vim behavior but it is
;; not appropriate in some cases like terminals."
;;   (setq-local evil-move-cursor-back nil))

;; (add-hook 'vterm-mode-hook #'evil-collection-vterm-escape-stay)

;; (define-key
;;  vterm-mode-map (kbd "<C-backspace>")
;;  (lambda ()
;;    (interactive)
;;    (vterm-send-key (kbd "C-w"))))
