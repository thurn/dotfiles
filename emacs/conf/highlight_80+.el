;; Enable highlighting long lines

(require 'highlight-80+)

 ;;;###autoload
(defun dthurn-turn-on-highlight-80+-mode ()
  "Turns on highlight-80+ mode if the buffer is appropriate."
  (if (not (or (window-minibuffer-p)
               buffer-read-only
               (eq major-mode 'eshell-mode)
               (eq major-mode 'shell-mode)))
      (highlight-80+-mode t)))

(define-global-minor-mode dthurn-highlight-80+-global-mode
  highlight-80+-mode
  dthurn-turn-on-highlight-80+-mode)
(dthurn-highlight-80+-global-mode t)
