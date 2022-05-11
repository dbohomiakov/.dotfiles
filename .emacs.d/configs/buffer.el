(use-package perspective
  :bind
  ("C-x C-b" . persp-list-buffers)   ; or use a nicer switcher, see below
  :custom
  (persp-suppress-no-prefix-key-warning t)
  :init
  (persp-mode))
