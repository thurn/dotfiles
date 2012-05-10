(defvar nimbus-keywords-regexp
  (regexp-opt
   '("def" "if" "for" "while" "unless" "return" "from" "import" "elif"
     "else" "catch" "throw" "until" "print" "try" "in" "True" "False"
     "del")
   'words))

(setq nimbus-font-lock-keywords
  `((,nimbus-keywords-regexp . font-lock-keyword-face)))

(define-derived-mode nimbus-mode lisp-mode "Nimbus Mode"
  "Major mode for editing Nimbus code"
  (setq font-lock-defaults '((nimbus-font-lock-keywords))))
