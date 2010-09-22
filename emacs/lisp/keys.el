;; Contains global Emacs key remappings.

;; Make it possible to execute extended commands without hitting ALT
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; I always mistype these
(global-set-key (kbd "C-x f") 'ido-find-file)
(global-set-key (kbd "C-x m") 'execute-extended-command)
(global-set-key (kbd "C-x b") 'ido-switch-buffer)

;; Use C-w as backward-kill-word and use C-x C-k as kill-region
;;(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)

;; Use command for meta on mac
(setq mac-command-modifier 'meta)
