(require 'cl-lib)
(require 'color)
(require 'rainbow-delimiters)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(cl-loop
 for index from 1 to rainbow-delimiters-max-face-count
 do
 (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
   (cl-callf color-saturate-name (face-foreground face) 100)))

(set-face-attribute 'rainbow-delimiters-unmatched-face nil
                    :foreground 'unspecified
                    :inherit 'error
                    :strike-through t)
