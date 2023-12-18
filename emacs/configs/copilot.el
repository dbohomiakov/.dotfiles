(use-package
 copilot
 :straight
 (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
 :ensure t
 :custom
 (copilot-node-executable
  "/home/dbohomiakov/.asdf/installs/nodejs/18.18.2/bin/node"))
