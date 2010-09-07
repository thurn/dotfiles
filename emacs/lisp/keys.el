;; Contains global Emacs key remappings.

;; I use Control-Space to open up Quicksilver, so a different command is needed
;; for setting the mark
(global-set-key (kbd "C-;") 'set-mark-command)

;; Make it possible to execute extended commands without hitting ALT
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; I always mistype these
(global-set-key (kbd "C-x f") 'ido-find-file)
(global-set-key (kbd "C-x m") 'execute-extended-command)
(global-set-key (kbd "C-x b") 'ido-switch-buffer)

;; Use C-w as backward-kill-word and use C-x C-k as kill-region
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)

(global-set-key (kbd "C-x w") 'ido-switch-buffer-other-window)

;; Use command for meta on mac
(setq mac-command-modifier 'meta)
