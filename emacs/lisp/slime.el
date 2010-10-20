;; Configuration for slime - the Superior Lisp Interaction Mode for Emacs

;; (setq inferior-lisp-program "/opt/local/bin/sbcl") ; Lisp system
;(require 'slime)
;(slime-setup)

;; clojure-mode
(require 'clojure-mode)

;; slime
(eval-after-load "slime" 
  '(progn (slime-setup '(slime-repl))))

(require 'slime)
(slime-setup)

(add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)