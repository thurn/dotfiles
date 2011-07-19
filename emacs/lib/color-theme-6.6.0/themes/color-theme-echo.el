(defun color-theme-echo-glossy () 
  "The Echo (Glossy) color theme for Emacs" 
  (interactive) 
  (color-theme-install 
   '(color-theme-echo-glossy
     ((background-color . "#222222") 
      (background-mode . dark) 
      (border-color . "#222222") 
      (cursor-color . "white") 
      (foreground-color . "white") 
      (list-matching-lines-face . bold) 
      (view-highlight-face . highlight)) 
     (default ((t (nil)))) 
     (bold ((t (:bold t)))) 
     (bold-italic ((t (:italic t :bold t)))) 
     (fringe ((t (:background "#222222")))) 
     (font-lock-builtin-face ((t (:foreground: "#aaccff")))) 
     (font-lock-comment-face ((t (:foreground "#aa44dd")))) 
     (font-lock-comment-delimiter-face ((t (:foreground "#aa44dd")))) 
     (font-lock-constant-face ((t (:foreground "white")))) 
     (font-lock-function-name-face ((t (:foreground "#ffcc00")))) 
     (font-lock-keyword-face ((t (:foreground "#ff6600")))) 
     (font-lock-preprocessor-face ((t (:foreground "#aaffff")))) 
     (font-lock-reference-face ((t (:foreground "LightSteelBlue")))) 
     (font-lock-string-face ((t (:foreground "#66FF00")))) 
     (font-lock-doc-face ((t (:foreground "LightSalmon")))) 
     (font-lock-type-face ((t (:foreground "#ff88bb")))) 
     (font-lock-variable-name-face ((t (:foreground "#aaccff")))) 
     (font-lock-warning-face
      ((t (:foreground "white"))))
     (font-lock-error-face
      ((t (:foreground "white"))))
     (paren-face-match-light ((t (:background "#777777")))) 
     (highlight ((t (:background "darkolivegreen")))) 
     (italic ((t (:italic t)))) 
     (modeline ((t (:background "#a5baf1" :foreground "black")))) 
     (modeline-buffer-id ((t (:background "#a5ba1" :foreground 
"black")))) 
     (modeline-mousable ((t (:background "#a5baf1" :foreground 
"black")))) 
     (modeline-mousable-minor-mode ((t (:background 
"#a5baf1" :foreground "black")))) 
     (region ((t (:background "#555577")))) 
     (primary-selection ((t (:background "#777777")))) 
     (isearch ((t (:background "#777777")))) 
     (zmacs-region ((t (:background "#777777")))) 
     (secondary-selection ((t (:background "darkslateblue")))) 
     (flymake-errline ((t (:background "LightSalmon" :foreground 
"black")))) 
     (flymake-warnline ((t (:background "LightSteelBlue" :foreground 
"black")))) 
     (underline ((t (:underline t)))) 
     (minibuffer-prompt ((t (:bold t :foreground "#ff6600"))))))) 

(defun color-theme-echo-matte () 
  "The Echo (Matte) color theme for Emacs" 
  (interactive) 
  (color-theme-install 
   '(color-theme-echo-matte
     ((background-color . "black") 
      (background-mode . dark) 
      (border-color . "black") 
      (cursor-color . "white") 
      (foreground-color . "white") 
      (list-matching-lines-face . bold) 
      (view-highlight-face . highlight)) 
     (default ((t (nil)))) 
     (bold ((t (:bold t)))) 
     (bold-italic ((t (:italic t :bold t)))) 
     (fringe ((t (:background "black")))) 
     (font-lock-builtin-face ((t (:foreground "#aaccff")))) 
     (font-lock-comment-face ((t (:foreground "#aa44dd")))) 
     (font-lock-comment-delimiter-face ((t (:foreground "#aa44dd")))) 
     (font-lock-constant-face ((t (:foreground "white")))) 
     (font-lock-function-name-face ((t (:foreground "#ffcc00")))) 
     (font-lock-keyword-face ((t (:foreground "#ff6600")))) 
     (font-lock-preprocessor-face ((t (:foreground "#aaffff")))) 
     (font-lock-reference-face ((t (:foreground "LightSteelBlue")))) 
     (font-lock-string-face ((t (:foreground "#66FF00")))) 
     (font-lock-doc-face ((t (:foreground "LightSalmon")))) 
     (font-lock-type-face ((t (:foreground "#ff88bb")))) 
     (font-lock-variable-name-face ((t (:foreground "#aaccff")))) 
     (font-lock-warning-face
      ((t (:foreground "white"))))
     (font-lock-error-face
      ((t (:foreground "white"))))
     (paren-face-match-light ((t (:background "#777777")))) 
     (highlight ((t (:background "darkolivegreen")))) 
     (italic ((t (:italic t)))) 
     (modeline ((t (:background "#a5baf1" :foreground "black")))) 
     (modeline-buffer-id ((t (:background "#a5ba1" :foreground 
"black")))) 
     (modeline-mousable ((t (:background "#a5baf1" :foreground 
"black")))) 
     (modeline-mousable-minor-mode ((t (:background 
"#a5baf1" :foreground "black")))) 
     (region ((t (:background "#555577")))) 
     (primary-selection ((t (:background "#777777")))) 
     (isearch ((t (:background "#777777")))) 
     (zmacs-region ((t (:background "#777777")))) 
     (secondary-selection ((t (:background "darkslateblue")))) 
     (flymake-errline ((t (:background "LightSalmon" :foreground 
"black")))) 
     (flymake-warnline ((t (:background "LightSteelBlue" :foreground 
"black")))) 
     (underline ((t (:underline t)))) 
     (minibuffer-prompt ((t (:bold t :foreground "#ff6600"))))))) 
