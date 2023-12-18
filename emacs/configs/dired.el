(use-package
 dired
 :straight (:type built-in)
 :commands (dired dired-jump)
 :bind
 (:map
  dired-mode-map
  ("<normal-state> h" . dired-up-directory)
  ("<normal-state> l" . dired-single-buffer))
 :delight "Dired"
 :custom
 (dired-auto-revert-buffer t)
 (dired-dwim-target t)
 (dired-hide-details-hide-symlink-targets nil)
 (dired-listing-switches "-alh --group-directories-first")
 (dired-ls-F-marks-symlinks nil)
 (dired-recursive-copies 'always))

(use-package
 dired-single
 :after dired
 :bind
 (:map
  dired-mode-map
  ([remap dired-find-file] . dired-single-buffer)
  ([remap dired-up-directory] . dired-single-up-directory)
  ("M-DEL" . dired-prev-subdir)))

(use-package
 dired-subtree
 :after dired
 :custom (dired-subtree-line-prefix "  ")
 :bind (:map dired-mode-map ("<tab>" . dired-subtree-toggle)))

(use-package
 dired-hide-dotfiles
 :hook (dired-mode . dired-hide-dotfiles-mode)
 :bind
 (:map
  dired-mode-map ("<normal-state> H" . dired-hide-dotfiles-mode)))

(use-package dired-narrow :ensure nil)

(use-package consult-dir)
