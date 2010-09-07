;; Configuration for Rainbow Parens mode.

(require 'rainbow-parens)
(add-hook 'clojure-mode-hook 'rainbow-paren-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-paren-mode)
(add-hook 'lisp-mode-hook 'rainbow-paren-mode)
(add-hook 'lisp-iteraction-mode-hook 'rainbow-paren-mode)

