(use-package perspective
  :straight t
  :bind
  ("C-x C-b" . persp-list-buffers)   ; or use a nicer switcher, see below
  :init
  (persp-mode))
