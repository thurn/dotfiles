(add-hook 'prog-mode-hook 'eldoc-mode)
(setq cider-prompt-for-symbol nil)
(setq cider-use-overlays nil)
(setq cider-show-error-buffer nil)
(setq cider-refresh-show-log-buffer nil)

(setq cider-refresh-before-fn "cider/stop!"
      cider-refresh-after-fn "cider/start!")
