;; Configuration for revive.el, for saving window configuration.

(autoload 'save-current-configuration "revive" "Save status" t)
(autoload 'resume "revive" "Resume Emacs" t)
(autoload 'wipe "revive" "Wipe Emacs" t)


(defun dthurn-resume (&rest args)
  (interactive)
  (kill-matching-buffers "scratch buffer")
  (resume))

;; C-c v to save windows, C-c r to resume them
(global-set-key (kbd "C-c v") 'save-current-configuration)
(global-set-key (kbd "C-c b") 'dthurn-resume)

(add-hook 'auto-save-hook 'save-current-configuration)
(add-hook 'kill-emacs-hook 'save-current-configuration)