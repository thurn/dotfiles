;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The entry point into the magical world that is my emacs configurations.    ;;
;; This file defines some helper functions for loading other elisp files and  ;;
;; then loads all of the configuration directories.                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-directory-dthurn (dir)
  "Loads every .el file in a directory in sorted order"
  (mapcar 'load-file (directory-files dir t "\\.el\\'")))

(defun recursive-add-to-load-path-dthurn (dir)
  "Adds the supplied directory to the load-path, as well as an of its subdirectories"
  (let ((default-directory dir))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))

;; Set up ELPA, the Emacs Lisp Package Archive
(when
    (load
     (concat emacs-root-dthurn "elpa/package.el"))
  (package-initialize))

;; Set load path to be the site-lisp directory and all of its subdirectories. This 
;; directory holds all of my emacs libraries.
(recursive-add-to-load-path-dthurn (concat emacs-root-dthurn "site-lisp"))

;; Load customizations. NOTE: Sometimes, the order of customization loading is 
;; important. To enable an ordering, the customizations are always loaded by 
;; dthurn-load-directory above in alphabetical order as specified by lessp. 
;; When customizations need to be ordered, prefix the file containing them
;; with aa to put them at the start of the load sequence or zz to put them
;; at the end of the sequence, then differentiate among them using a digits.
;; For example, to make visited.el load last, name it zz1_visited.el
(load-directory-dthurn (concat emacs-root-dthurn "lisp"))
