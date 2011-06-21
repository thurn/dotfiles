;; dthurn's .emacs file.
;; 
;; Default .emacs file. Attempts to make an intelligent guess as
;; to the location of the root of my customizations.

(defconst init-file-dthurn "~/emacs/init.el" 
     "The file to bootstrap my emacs configuration.")

(defconst dthurn-local-config "~/.emacs_config.el"
     "Local configuration")

(if (file-exists-p dthurn-local-config)
  (load-file dthurn-local-config))

(load-file init-file-dthurn)

