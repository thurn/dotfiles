;; dthurn's .emacs file.
;; 
;; Default .emacs file. Attempts to make an intelligent guess as
;; to the location of the root of my customizations.

(defvar emacs-root-dthurn (if (or (eq system-type 'ms-dos)
    (eq system-type 'windows-nt))
        "C:/emacs/" ;; Windows root
        "~/emacs/" ) ;; UNIX root
     "A guess at where the emacs root is located.")

(defvar init-file-dthurn "init.el" 
     "The file to bootstrap my emacs configuration.")

(defvar dthurn-local-config ".emacs_config"
     "Local configuration")

(load-file (concat emacs-root-dthurn init-file-dthurn))

(if (file-exists-p dthurn-local-config)
  (load-file dthurn-local-config))
