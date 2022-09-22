(use-package restclient)

(use-package ob-restclient
  :straight (:host github :repo "alf/ob-restclient.el"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (restclient . t)
   (ipython . t)
   (shell . t)
   (python . t)
   (org . t)
   (sql . t)))
