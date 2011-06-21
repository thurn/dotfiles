;; Configuration for Haskell Mode for Emacs

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

;; Pick one of these three indentation modes
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
