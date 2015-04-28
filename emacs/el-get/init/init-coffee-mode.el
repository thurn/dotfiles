;; Configuration for coffeescript

(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(setq coffee-tab-width 2)
(setq coffee-assign-regexp "\\(\\(\\w\\|\\.\\|_\\|$\\)+?\s*\\)=")
