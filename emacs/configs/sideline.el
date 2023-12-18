(use-package sideline-blame)
(use-package sideline-flymake)

(use-package sideline
  :hook (flymake-mode . sideline-mode)
  :init
  ;; (setq sideline-backends-left '((sideline-blame . down)))
  (setq sideline-backends-right '(sideline-flymake))
  :custom
  (sideline-flymake-display-mode 'point))
