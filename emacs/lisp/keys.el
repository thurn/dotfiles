;; Contains global Emacs key remappings.

;; I use Control-Space to open up Quicksilver, so a different command is needed
;; for setting the mark
;;(global-set-key (kbd "C-;") 'set-mark-command)

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

;; (global-set-key (kbd "C-j") 'next-line)
;; (global-set-key (kbd "C-f") 'previous-line)
;; (global-set-key (kbd "C-k") 'forward-word)
;; (global-set-key (kbd "C-d") 'forward-char)
;; (global-set-key (kbd "C-l") 'backward-word)
;; (global-set-key (kbd "C-s") 'backward-char)
;; (global-set-key (kbd "C-;") 'isearch-forward)
;; (global-set-key (kbd "C-a") 'dthurn-cycle-bol)
;; (global-set-key (kbd "C-v") 'backward-kill-word)
;; (global-set-key (kbd "C-i") 'yank)
;; (global-set-key (kbd "C-e") 'save-buffer)
;; (global-set-key (kbd "C-o") 'kill-line)
;; (global-set-key (kbd "C-w") 'scroll-up)
;; (global-set-key (kbd "C-g") 'other-window)
;; (global-set-key (kbd "C-n") 'end-of-line)
;; (global-set-key (kbd "C-,") 'ido-find-file)
;; (global-set-key (kbd "C-u") 'scroll-down)
;; (global-set-key (kbd "C-r") 'execute-extended-command)
;; (global-set-key (kbd "C-'") 'ido-kill-buffer)
;; (global-set-key (kbd "C-t") 'set-mark-command)
;; (global-set-key (kbd "C-b") 'kill-word)
;; (global-set-key (kbd "C-.") 'isearch-backward)
;; (global-set-key (kbd "C-/") 'undo)
;; (global-set-key (kbd "C-z") 'kill-region-save)
;; (global-set-key (kbd "C-q") 'ido-switch-buffer)
;; (global-set-key (kbd "C-p") 'kill-region)
;; (global-set-key (kbd "C-y") 'keyboard-quit)

;; Use command for meta on mac
(setq mac-command-modifier 'meta)
