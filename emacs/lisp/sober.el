;; Provides sober-mode, giving you Sober keybindings as a minor mode

(defvar sober-mode-map (make-keymap)
 "Keymap for sober-mode.")

(define-key sober-mode-map (kbd "C-j") 'next-line)
(define-key sober-mode-map (kbd "C-f") 'previous-line)
(define-key sober-mode-map (kbd "C-k") 'forward-word)
(define-key sober-mode-map (kbd "C-d") 'forward-char)
(define-key sober-mode-map (kbd "C-l") 'backward-word)
(define-key sober-mode-map (kbd "M-z") 'backward-char) ;; C-;
(define-key sober-mode-map (kbd "C-a") 'beginning-of-line)
(define-key sober-mode-map (kbd "C-r") 'backward-kill-word)
(define-key sober-mode-map (kbd "C-u") 'yank)
(define-key sober-mode-map (kbd "C-v") 'save-buffer)
(define-key sober-mode-map (kbd "C-n") 'kill-line)
(define-key sober-mode-map (kbd "M-p") 'scroll-up) ;; C-i
(define-key sober-mode-map (kbd "C-e") 'delete-char)
(define-key sober-mode-map (kbd "C-t") 'other-window)
(define-key sober-mode-map (kbd "M-x") 'end-of-line) ;; C-m
(define-key sober-mode-map (kbd "C-o") 'ido-find-file)
(define-key sober-mode-map (kbd "C-w") 'forward-sexp)
(define-key sober-mode-map (kbd "M-q") 'execute-extended-command) ;; C-,
(define-key sober-mode-map (kbd "C-p") 'ido-switch-buffer-other-window)

(define-key sober-mode-map (kbd "M-j") 'ido-kill-buffer)
(define-key sober-mode-map (kbd "M-f") 'scroll-down)
(define-key sober-mode-map (kbd "M-k") 'set-mark-command)
(define-key sober-mode-map (kbd "M-d") 'kill-word)
(define-key sober-mode-map (kbd "M-l") 'isearch-backward)
(define-key sober-mode-map (kbd "M-s") 'copy-region-as-kill)
(define-key sober-mode-map (kbd "M-;") 'ido-switch-buffer)
(define-key sober-mode-map (kbd "M-a") 'kill-region)
(define-key sober-mode-map (kbd "M-r") 'forward-paragraph)
(define-key sober-mode-map (kbd "M-u") 'beginning-of-buffer)
(define-key sober-mode-map (kbd "M-g") 'end-of-buffer)
(define-key sober-mode-map (kbd "M-h") 'backward-sexp)
(define-key sober-mode-map (kbd "M-v") 'delete-other-windows)
(define-key sober-mode-map (kbd "M-n") 'backward-paragraph)

(define-key sober-mode-map (kbd "C-S-j") 'ido-kill-buffer)
(define-key sober-mode-map (kbd "C-S-f") 'scroll-down)
(define-key sober-mode-map (kbd "C-S-k") 'set-mark-command)
(define-key sober-mode-map (kbd "C-S-d") 'kill-word)
(define-key sober-mode-map (kbd "C-S-l") 'isearch-backward)
(define-key sober-mode-map (kbd "C-S-s") 'copy-region-as-kill)
(define-key sober-mode-map (kbd "C-S-;") 'ido-switch-buffer)
(define-key sober-mode-map (kbd "C-S-a") 'kill-region)
(define-key sober-mode-map (kbd "C-S-r") 'forward-paragraph)
(define-key sober-mode-map (kbd "C-S-u") 'beginning-of-buffer)
(define-key sober-mode-map (kbd "C-S-g") 'end-of-buffer)
(define-key sober-mode-map (kbd "C-S-h") 'backward-sexp)
(define-key sober-mode-map (kbd "C-S-v") 'delete-other-windows)
(define-key sober-mode-map (kbd "C-S-n") 'backward-paragraph)


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
