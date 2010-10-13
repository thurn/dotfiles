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

(defvar shell-path-dthurn "/bin/zsh"
     "A guess at where my default shell is located.")

(load-file (concat emacs-root-dthurn init-file-dthurn))

;; Start the emacs server for emacsclient to connect to
(server-start)
