(use-package
  copilot
  :straight
  (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :custom
  (copilot-node-executable
   "/home/dmytro/.asdf/installs/nodejs/18.19.1/bin/node"))


(use-package
  copilot-chat
  :straight
  (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :after (request org markdown-mode shell-maker)
  :custom (copilot-chat-frontend 'org))

(add-hook 'git-commit-setup-hook 'copilot-chat-insert-commit-message)

;; (use-package
;;  aider
;;  :straight (:host github :repo "tninja/aider.el" :files ("aider.el"))
;;  :config
;;  (setq aider-args '("--model" "gpt-4o-mini"))
;;  (setenv "OPENAI_API_KEY" "")
;;  ;; Optional: Set a key binding for the transient menu
;;  (global-set-key (kbd "C-c a") 'aider-transient-menu))
