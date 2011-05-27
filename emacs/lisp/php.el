(autoload 'xhp-mode "xhp-mode"
  "Major mode for editing PHP code including XHP support." t)

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
