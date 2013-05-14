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

;; Set up El-Get
(add-to-list 'load-path "~/emacs/el-get/el-get")
(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))
(add-to-list 'el-get-recipe-path "~/emacs/el-get/recipes")
(add-to-list  'package-archives
  '("marmalade" . "http://marmalade-repo.org/packages/"))
(setq el-get-user-package-directory "~/emacs/el-get/init")
(el-get 'sync)

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
