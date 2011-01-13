;; Configuration for ParEdit

(require 'paredit)
(autoload 'paredit-mode "paredit"
      "Minor mode for pseudo-structurally editing Lisp code." t)

;; Don't really want automatic paredit mode
;(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
;(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
;(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
;(add-hook 'clojure-mode-hook          (lambda () (paredit-mode +1)))
