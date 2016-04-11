;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The entry point into the magical world that is my emacs configurations.    ;;
;; This file defines some helper functions for loading other elisp files and  ;;
;; then loads all of the configuration directories.                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-directory-dthurn (dir)
  "Loads every .el file in a directory in sorted order"
  (mapcar 'load-file (directory-files dir t "\\.el\\'")))

(defun recursive-add-to-load-path-dthurn (dir)
  "Adds the supplied directory to the load-path, as well as an of its
   subdirectories"
  (let ((default-directory dir))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))

(setq user-emacs-directory "~/emacs/")

;; Set the load path
(if (not (getenv "TERM_PROGRAM"))
    (let ((path (shell-command-to-string
            "$SHELL -cl \"printf %s \\\"\\\$PATH\\\"\"")))
      (setenv "PATH" path)))
(setq exec-path (split-string (getenv "PATH") ":"))

(package-initialize)

;; Set up El-Get
(add-to-list 'load-path "~/emacs/el-get/el-get")
(require 'el-get)

(if (boundp 'el-get-recipe-path)
  (add-to-list 'el-get-recipe-path "~/emacs/el-get/recipes"))
(if (boundp 'package-archives)
  (add-to-list 'package-archives
    '("melpa" . "http://melpa.milkbox.net/packages/")
    '("marmalade" . "http://marmalade-repo.org/packages/")))
(setq el-get-user-package-directory "~/emacs/el-get/init")

; ido-ubiquitous?
(setq dthurn-el-get-packages
  '(command-frequency textmate undo-tree hungry-delete groovy-emacs-mode
    cider clojure-mode eval-sexp-fu markdown-mode
    smartparens rainbow-delimiters company))

(el-get 'sync dthurn-el-get-packages)

;; Set load path to be the site-lisp directory and all of its subdirectories.
;; This directory holds all of my emacs libraries.
(recursive-add-to-load-path-dthurn "~/emacs/lib")

;; Load customizations. NOTE: Sometimes, the order of customization loading is
;; important. To enable an ordering, the customizations are always loaded by
;; dthurn-load-directory above in alphabetical order as specified by lessp.
;; When customizations need to be ordered, prefix the file containing them
;; with aa to put them at the start of the load sequence or z to put them
;; at the end of the sequence, then differentiate among them using digits.
;; For example, to make visited.el load last, name it z1_visited.el
(load-directory-dthurn "~/emacs/conf")

(if (file-exists-p "~/.emacs_config.el")
  (load-file "~/.emacs_config.el"))

;; The final directory contains .el files that should be loaded as late as
;; possible in the startup process. The same rule about alphabetical loading
;; applies to final.
(load-directory-dthurn "~/emacs/final")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-process-type (quote ghci))
 '(haskell-stylish-on-save t)
 '(haskell-tags-on-save t)
 '(python-indent-offset 2)
 '(safe-local-variable-values (quote ((eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook" (add-hook (quote write-contents-functions) (lambda nil (delete-trailing-whitespace) nil)) (require (quote whitespace)) "Sometimes the mode needs to be toggled off and on." (whitespace-mode 0) (whitespace-mode 1)) (whitespace-line-column . 80) (whitespace-style face trailing lines-tail) (require-final-newline . t)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
