;; dthurn's .emacs file.

(if (file-exists-p "~/emacs/init.el")
    (load-file "~/emacs/init.el"))

(if (file-exists-p "~/.emacs_config.el")
  (load-file "~/.emacs_config.el"))
