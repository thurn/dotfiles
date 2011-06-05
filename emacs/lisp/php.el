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

;; Automatically enable xhp-mode
(setq magic-mode-alist (append '(("<\\?php\\s " . xhp-mode))
                              magic-mode-alist))
(setq auto-mode-alist (append '(("\\.php$" . xhp-mode))
                              auto-mode-alist))
