;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The entry point into the magical world that is my emacs configurations.    ;;
;; This file defines some helper functions for loading other elisp files and  ;;
;; then loads all of the configuration directories.                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set load path
(add-to-list 'load-path (concat emacs-root "site-lisp"))

(load-file (concat emacs-root "global.el"))
(load-file (concat emacs-root "emacs.el"))
(load-file (concat emacs-root "vimpulse.el"))
(load-file (concat emacs-root "js2.el"))
(load-file (concat emacs-root "term.el"))
(load-file (concat emacs-root "tramp.el"))
(load-file (concat emacs-root "visited.el"))
(load-file (concat emacs-root "aquamacs.el"))

