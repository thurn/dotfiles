;; php-specific stuff

(require 'xhp-mode)
(autoload 'xhp-mode "xhp-mode"
  "Major mode for editing PHP code including XHP support." t)

;;=========================================================
;;PHP Indentation Style
;;========================================================
(defconst fb-php-style
  '((c-basic-offset . 2)
    (c-offsets-alist . (
                        (arglist-intro . +)
                        (case-label . +)
                        (arglist-close . c-lineup-close-paren)
                        )))
  "PHP Programming style"
  )
(c-add-style "fb-php-style" fb-php-style)
(unless (boundp 'xhp-mode-hook) (setq xhp-mode-hook '()))
(add-to-list 'xhp-mode-hook
             (lambda () (c-toggle-electric-state -1)))

(define-key xhp-mode-map (kbd "<tab>") 'dthurn-tab)
(define-key xhp-mode-map (kbd "S-<tab>") 'dthurn-backward-tab)
(define-key xhp-mode-map (kbd "M-<tab>") 'indent-relative-maybe)
