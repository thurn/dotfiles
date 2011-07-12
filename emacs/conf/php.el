;; php-specific stuff

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
;(add-to-list 'xhp-mode-hook (lambda () (flymake-mode 1)))
