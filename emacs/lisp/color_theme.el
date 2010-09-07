;; Configuration for color-theme, emacs syntax highlighting.

(require 'color-theme)
(require 'zenburn)

(eval-after-load "color-theme"
  '(progn (color-theme-initialize)
          (color-theme-zenburn)))

(unless (zenburn-format-spec-works-p)
  (zenburn-define-format-spec))

