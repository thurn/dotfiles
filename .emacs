;; Default .emacs file. Attempts to make an intelligent guess as
;; to the location of the root of my customizations.

(defvar emacs-root (if (or (eq system-type 'ms-dos)
			   (eq system-type 'windows-nt))
		           "C:/emacs/" ;; Windows root
		           "~/emacs/" ) ;; UNIX root
  "A guess at where the emacs root is located.")

(defvar init-file "init.el" 
  "The file to bootstrap my emacs configuration.")

(load-file (concat emacs-root init-file))
