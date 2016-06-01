(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

(setq company-idle-delay 0.1)

;(global-set-key (kbd "TAB") #'company-indent-or-complete-common)
