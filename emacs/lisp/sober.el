;; Provides sober-mode, giving you Sober keybindings as a minor mode

(defvar sober-mode-map (make-keymap)
 "Keymap for sober-mode.")

(define-key sober-mode-map (kbd "C-j") 'next-line)
(define-key sober-mode-map (kbd "C-f") 'previous-line)
(define-key sober-mode-map (kbd "C-k") 'forward-word)
(define-key sober-mode-map (kbd "C-d") 'forward-char)
(define-key sober-mode-map (kbd "C-l") 'backward-word)
(define-key sober-mode-map (kbd "C-s") 'backward-char)
(define-key sober-mode-map (kbd "C-;") 'isearch-forward)
(define-key sober-mode-map (kbd "C-v") 'backward-kill-word)
(define-key sober-mode-map (kbd "C-o") 'save-buffer)
(define-key sober-mode-map (kbd "C-e") 'yank)
(define-key sober-mode-map (kbd "C-w") 'kill-line)
(define-key sober-mode-map (kbd "C-g") 'scroll-up)
(define-key sober-mode-map (kbd "C-n") 'other-window)
(define-key sober-mode-map (kbd "C-,") 'end-of-line)
(define-key sober-mode-map (kbd "C-u") 'ido-find-file)
(define-key sober-mode-map (kbd "C-r") 'scroll-down)
(define-key sober-mode-map (kbd "C-'") 'execute-extended-command)
(define-key sober-mode-map (kbd "C-t") 'ido-kill-buffer)
(define-key sober-mode-map (kbd "C-b") 'set-mark-command)
(define-key sober-mode-map (kbd "C-.") 'kill-word)
(define-key sober-mode-map (kbd "C-/") 'undo)
(define-key sober-mode-map (kbd "C-z") 'copy-region-as-kill)
(define-key sober-mode-map (kbd "C-q") 'ido-switch-buffer)
(define-key sober-mode-map (kbd "C-p") 'kill-region)
(define-key sober-mode-map (kbd "C-y") 'keyboard-quit)
(define-key sober-mode-map (kbd "C-0") 'forward-sexp)
(define-key sober-mode-map (kbd "C-9") 'isearch-backward)

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