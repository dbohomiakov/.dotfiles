(use-package
 verb
 :config
 (add-to-list
  'verb-content-type-handlers
  '("application/hal\\+json" verb-handler-json)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ipython . t)
   (shell . t)
   (python . t)
   (org . t)
   (sql . t)
   (verb . t)))
