(require 'parinfer)

(setq parinfer-auto-switch-indent-mode t)

(setq parinfer-extensions '(defaults pretty-parens smart-yank))

(add-hook 'clojure-mode-hook 'parinfer-mode)
