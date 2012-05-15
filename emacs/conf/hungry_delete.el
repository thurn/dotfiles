;; Configuration for hungry-delete mode
(require 'hungry-delete)
(define-global-minor-mode
  dthurn-hungry-delete-global-mode
  hungry-delete-mode
  turn-on-hungry-delete-mode)
(dthurn-hungry-delete-global-mode t)
(add-hook 'c-mode-common-hook
          (lambda () (interactive) (c-toggle-hungry-state t)))
