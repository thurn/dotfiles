;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration for js2-mode JavaScript IDE.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable js2-mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; js2-mode configuration
(setq js2-allow-keywords-as-property-names nil)
(setq js2-auto-indent-p nil)
(setq js2-basic-offset 2)
(setq js2-bounce-indent-p t)
(setq js2-cleanup-whitespace t)
(setq js2-enter-indents-newline nil)
(setq js2-global-externs (quote ("algjs" "$" "goog" "YAHOO" "jQuery")))
(setq js2-highlight-level 3)
(setq js2-idle-timer-delay 0.2)
(setq js2-indent-on-enter-key nil)
(setq js2-mirror-mode nil)

