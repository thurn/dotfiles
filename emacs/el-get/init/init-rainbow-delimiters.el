(require 'cl-lib)
(require 'color)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground "red")
(set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground "yellow")
(set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground "green")
(set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground "cyan")
(set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground "orange")
(set-face-attribute 'rainbow-delimiters-depth-6-face nil :foreground "magenta")
(set-face-attribute 'rainbow-delimiters-depth-7-face nil :foreground "gray")
(set-face-attribute 'rainbow-delimiters-depth-8-face nil :foreground "cornflower blue")
(set-face-attribute 'rainbow-delimiters-depth-9-face nil :foreground "red")

(set-face-attribute 'rainbow-delimiters-unmatched-face nil
                    :foreground 'unspecified
                    :inherit 'error
                    :strike-through t)
