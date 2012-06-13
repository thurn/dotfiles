;; Configuration for Haskell Mode for Emacs

(require 'hs-lint)

(load "~/emacs/lib/haskell-mode/haskell-site-file.el")

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)

;(define-key haskell-mode-map (kbd "C-c M-t") 'inferior-haskell-info)
;(define-key haskell-mode-map (kbd "C-c C-\\") 'haskell-indent-insert-guard)
