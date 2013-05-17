;; Configuration for auto-complete-mode

(require 'auto-complete)
(require 'auto-complete-config)

(ac-config-default)

;; Enable auto-complete
(auto-complete-mode t)

;;;###autoload
(defun dthurn-turn-on-auto-complete-mode ()
  "Turns on auto-complete mode if the buffer is appropriate."
  (if (not (or (window-minibuffer-p)
               (minibufferp)
               buffer-read-only
               (eq major-mode 'shell-mode)))
      (auto-complete-mode t)))

(define-global-minor-mode dthurn-auto-complete-global-mode
  auto-complete-mode
  dthurn-turn-on-auto-complete-mode)
(dthurn-auto-complete-global-mode t)
