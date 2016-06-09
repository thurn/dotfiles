(require 'rainbow-delimiters)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(set-face-attribute 'rainbow-delimiters-unmatched-face nil
                     :foreground 'unspecified
                     :inherit 'error
                     :strike-through t)
