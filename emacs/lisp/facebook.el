;; Facebook-specific stuff. Don't commit to github!

(setq large-file-warning-threshold nil)
(visit-tags-table "~/Documents/www/ETAGS")

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
  "Facebook's PHP Programming style"
)
(c-add-style "fb-php-style" fb-php-style)

;; Automatically enable xhp-mode
(setq magic-mode-alist (append '(("<\\?php\\s " . xhp-mode))
                              magic-mode-alist))
(setq auto-mode-alist (append '(("\\.php$" . xhp-mode))
                              auto-mode-alist))