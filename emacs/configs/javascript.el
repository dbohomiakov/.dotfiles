(use-package
 js-mode
 :straight (:type built-in)
 :hook ((js-mode . eglot-ensure)))
