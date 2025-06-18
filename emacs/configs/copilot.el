(use-package
 copilot
 :straight
 (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
 :ensure t
 :custom
 (copilot-node-executable
  (concat db/home-dir "/.asdf/installs/nodejs/18.19.1/bin/node")))


(use-package
 copilot-chat
 :straight
 (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
 :after (request org markdown-mode shell-maker)
 :custom (copilot-chat-frontend 'org))

(add-hook 'git-commit-setup-hook 'copilot-chat-insert-commit-message)
