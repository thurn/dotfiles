;; Enable highlighting long lines

(require 'highlight-80+)

(add-hook 'php-mode-hook
          (lambda ()
            (c-set-style "fb-php-style")
            (highlight-80+-mode t)
            ))  

(add-hook 'xhp-mode-hook
          (lambda ()
            (c-set-style "fb-php-style")
            (highlight-80+-mode t)
            ))
