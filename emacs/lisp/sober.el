;; Provides sober-mode, giving you Sober keybindings as a minor mode

(defvar sober-mode-map (make-keymap)
 "Keymap for sober-mode.")
(define-key thor-mode-map (kbd "C-j") 'next-line)
(define-key thor-mode-map (kbd "C-k") 'forward-word)
(define-key thor-mode-map (kbd "C-l") 'backward-word)
(define-key thor-mode-map (kbd "C-f") 'previous-line)
(define-key thor-mode-map (kbd "C-d") 'forward-char)
(define-key thor-mode-map (kbd "C-s") 'backward-char)

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