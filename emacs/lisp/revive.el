;; Configuration for revive.el, for saving window configuration.

(autoload 'save-current-configuration "revive" "Save status" t)
(autoload 'resume "revive" "Resume Emacs" t)
(autoload 'wipe "revive" "Wipe Emacs" t)

;; C-c v to save windows, C-c r to resume them
(global-set-key (kbd "C-c v") 'save-current-configuration)
(global-set-key (kbd "C-c b") 'resume)
