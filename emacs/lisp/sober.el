;; Provides sober-mode, giving you Sober keybindings as a minor mode

(defvar sober-mode-map (make-keymap)
 "Keymap for sober-mode.")

(define-key sober-mode-map (kbd "C-j") 'forward-word)
(define-key sober-mode-map (kbd "C-l") 'backward-word)
(define-key sober-mode-map (kbd "C-;") 'save-buffer)
(define-key sober-mode-map (kbd "C-u") 'backward-kill-word)
(define-key sober-mode-map (kbd "C-o") 'ido-find-file)
(define-key sober-mode-map (kbd "C-,") 'forward-sexp)
(define-key sober-mode-map (kbd "C-'") 'other-window)
(define-key sober-mode-map (kbd "C-t") 'scroll-down)
(define-key sober-mode-map (kbd "C-.") 'switch-to-buffer-other-window)
(define-key sober-mode-map (kbd "C-z") 'kill-word)
(define-key sober-mode-map (kbd "C-S-j") 'set-mark-command)
(define-key sober-mode-map (kbd "C-S-f") 'execute-extended-command)
(define-key sober-mode-map (kbd "C-S-k") 'ido-kill-buffer)
(define-key sober-mode-map (kbd "C-S-d") 'delete-other-windows)
(define-key sober-mode-map (kbd "C-S-l") 'ido-switch-buffer)
(define-key sober-mode-map (kbd "C-S-s") 'copy-region-as-kill)
(define-key sober-mode-map (kbd "C-:") 'forward-paragraph)
(define-key sober-mode-map (kbd "C-S-a") 'beginning-of-buffer)
(define-key sober-mode-map (kbd "C-S-e") 'end-of-buffer)
(define-key sober-mode-map (kbd "C-S-g") 'backward-paragraph)
(define-key sober-mode-map (kbd "C-S-h") 'backward-sexp)

;;;###autoload
(define-minor-mode sober-mode
 "Minor mode to enable the sober keybinding system."
 :init-value nil
 :group 'sober)

;;;###autoload
(defun turn-on-sober-mode ()
 "Turns on sober mode if the buffer is appropriate."
 (sober-mode t))

(define-global-minor-mode sober-global-mode sober-mode turn-on-sober-mode)

(sober-global-mode t)
