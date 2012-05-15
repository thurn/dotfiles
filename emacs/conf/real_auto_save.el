; Real auto-save mode configuration

(require 'real-auto-save)
(add-hook 'text-mode-hook 'turn-on-real-auto-save)
(add-hook 'muse-mode-hook 'turn-on-real-auto-save)
(add-hook 'prog-mode-hook 'turn-on-real-auto-save)
(setq real-auto-save-interval 5) ;; in seconds
