(setq
user-emacs-directory
(expand-file-name "~/.cache/emacs/")
url-history-file
(expand-file-name "url/history" user-emacs-directory))

;; Move to top to fix package-selected-package
;; see https://github.com/jwiegley/use-package/issues/397
(setq custom-file (concat user-emacs-directory "custom.el"))

;; Disable package.el
(setq package-enable-at-startup nil)

(setq native-comp-eln-load-path
      `(,(expand-file-name "eln-cache/" user-emacs-directory)))
