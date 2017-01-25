(add-hook 'prog-mode-hook 'eldoc-mode)
(setq cider-prompt-for-symbol nil)
(setq cider-use-overlays nil)
(setq cider-show-error-buffer nil)

(setq cider-cljs-lein-repl
      "(do (use 'figwheel-sidecar.repl-api)
           (start-figwheel! \"main\" \"ui\")
           (cljs-repl \"ui\"))")
