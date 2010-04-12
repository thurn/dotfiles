;; Default .emacs file. Attempts to make an intelligent guess as
;; to the location of the root of my customizations.

(defvar emacs-root (if (or (eq system-type 'ms-dos)
			   (eq system-type 'windows-nt))
		           "C:/emacs/" ;; Windows root
		           "~/emacs/" ) ;; UNIX root
  "A guess at where the emacs root is located.")

(defvar init-file "init.el" 
  "The file to bootstrap my emacs configuration.")

(load-file (concat emacs-root init-file))
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(aquamacs-additional-fontsets (quote (("apple" "monaco*" "medium" "r" "normal" 9 "monaco") ("apple" "monaco*" "medium" "r" "normal" 10 "monaco") ("apple" "monaco*" "medium" "r" "normal" 11 "monaco") ("apple" "monaco*" "medium" "r" "normal" 12 "monaco") ("apple" "monaco*" "medium" "r" "normal" 13 "monaco") ("apple" "monaco*" "medium" "r" "normal" 14 "monaco") ("apple" "monaco*" "medium" "r" "normal" 16 "monaco") ("apple" "monaco*" "medium" "r" "normal" 18 "monaco") ("apple" "lucida grande*" "medium" "r" "normal" 9 "lucida") ("apple" "lucida grande*" "medium" "r" "normal" 10 "lucida") ("apple" "lucida grande*" "medium" "r" "normal" 11 "lucida") ("apple" "lucida grande*" "medium" "r" "normal" 12 "lucida") ("apple" "lucida grande*" "medium" "r" "normal" 13 "lucida") ("apple" "lucida grande*" "medium" "r" "normal" 14 "lucida") ("apple" "lucida grande*" "medium" "r" "normal" 16 "lucida") ("apple" "lucida grande*" "medium" "r" "normal" 18 "lucida") ("apple" "lucida sans typewrite*" "medium" "r" "normal" 9 "lucida_typewriter") ("apple" "lucida sans typewrite*" "medium" "r" "normal" 10 "lucida_typewriter") ("apple" "lucida sans typewrite*" "medium" "r" "normal" 12 "lucida_typewriter") ("apple" "lucida sans typewrite*" "medium" "r" "normal" 14 "lucida_typewriter") ("apple" "lucida console*" "medium" "r" nil 11 "lucida_console") (nil "courier*" "medium" "r" nil 11 "courier") (nil "courier*" "medium" "r" nil 13 "courier") (nil "bitstream vera sans mono" "medium" "r" "normal" 10 "vera_mono") (nil "bitstream vera sans mono" "medium" "r" "normal" 12 "vera_mono") (nil "bitstream vera sans mono" "medium" "r" "normal" 14 "vera_mono"))) t)
 '(aquamacs-customization-version-id 206 t)
 '(aquamacs-tool-bar-user-customization nil t)
 '(blink-cursor-mode nil)
 '(js2-allow-keywords-as-property-names nil)
 '(js2-auto-indent-p nil)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(js2-cleanup-whitespace t)
 '(js2-enter-indents-newline t)
 '(js2-global-externs (quote ("algjs")))
 '(js2-highlight-level 3)
 '(js2-idle-timer-delay 0.2)
 '(js2-indent-on-enter-key t)
 '(js2-mirror-mode nil)
 '(ns-tool-bar-display-mode (quote both) t)
 '(ns-tool-bar-size-mode (quote regular) t)
 '(show-paren-mode nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
