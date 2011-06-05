;; Configuration for auto-complete-mode
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories (concat emacs-root-dthurn "dict"))
(require 'auto-complete-config)
(ac-config-default)

 ;;;###autoload
(defun dthurn-turn-on-auto-complete-mode ()
  "Turns on auto-complete mode if the buffer is appropriate."
  (if (not (or (window-minibuffer-p)
               buffer-read-only
               (eq major-mode 'shell-mode)))
      (auto-complete-mode t)))

(setq ac-delay 0)

(define-global-minor-mode dthurn-auto-complete-global-mode
  auto-complete-mode
  dthurn-turn-on-auto-complete-mode)
(dthurn-auto-complete-global-mode t)