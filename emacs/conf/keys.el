;; Contains global Emacs key remappings.

;; Make it possible to execute extended commands without hitting ALT
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; I always mistype these
(global-set-key (kbd "C-x f") 'ido-find-file)
(global-set-key (kbd "C-x m") 'execute-extended-command)
(global-set-key (kbd "C-x b") 'ido-switch-buffer)
(global-set-key (kbd "C-x w") 'ido-switch-buffer-other-window)

(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)

(global-set-key (kbd "C-S-j") 'windmove-down)
(global-set-key (kbd "C-S-k") 'windmove-up)

;; Use command for meta on mac
(setq mac-command-modifier 'meta)
;; Fix osx clipboard
(setq x-select-enable-clipboard t)

;; Make keyboard-escape-quit not kill all windows
;; (defadvice keyboard-escape-quit
;;   (around my-keyboard-escape-quit activate)
;;   (flet ((one-window-p (&optional nomini all-frames) t)) ad-do-it))
