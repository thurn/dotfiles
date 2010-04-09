;; Set load path
(add-to-list 'load-path "~/.emacs.d/site-lisp")

(setq pop-up-frames nil)

;; Load vimpulse
(require 'vimpulse)

;; js2-mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Reload files from last session
(require 'save-visited-files)
(setq save-visited-files-auto-restore t)
(save-visited-files-mode t)

;;1
(vimpulse-map (kbd "C-k") 'viper-intercept-ESC-key)
(vimpulse-vmap (kbd "C-k") 'viper-intercept-ESC-key)
(vimpulse-imap (kbd "C-k") 'viper-intercept-ESC-key)

;;2
(vimpulse-vmap (kbd ";") 'viper-ex)
(vimpulse-map (kbd ";") 'viper-ex)

(vimpulse-vmap (kbd ":") 'viper-repeat-find)
(vimpulse-map (kbd ":") 'viper-repeat-find)
;;3
(vimpulse-vmap (kbd "s") 'save-buffer)
(vimpulse-map (kbd "s") 'save-buffer)
(vimpulse-vmap (kbd "C-s") 'viper-substitute)
(vimpulse-map (kbd "C-s") 'viper-substitute)


;;4
(vimpulse-map (kbd "H") 'viper-bol-and-skip-white)
(vimpulse-vmap (kbd "H") 'viper-bol-and-skip-white)
(vimpulse-map (kbd "L") 'viper-goto-eol)
(vimpulse-vmap (kbd "L") 'viper-goto-eol)

;;5
(vimpulse-map (kbd "gs")
              (lambda ()
                (interactive)
                (shell-command-on-region (point-min)
                                         (point-max)
                                         (concat "perl -p -i -e 's/"
                                                 (read-string ":%!perl -p -i -e 's/"))
                                         nil
                                         t)))

;; Block cursor
(setq-default cursor-type 'box)

;; Fixed C-u and C-d
(setq scroll-preserve-screen-position t)
(vimpulse-map (kbd "C-u") 'viper-scroll-down)
(vimpulse-vmap (kbd "C-u") 'viper-scroll-down)
(vimpulse-imap (kbd "C-u") 'viper-scroll-down) 

					;(defun nflath-viper-scroll-down (&optional arg)
					;(interactive)
					;(dotimes (i 20)
					;(scroll-down 1)
					;(previous-line)))
					;(vimpulse-map (kbd "C-u") 'nflath-viper-scroll-down)
					;(vimpulse-vmap (kbd "C-u") 'nflath-viper-scroll-down)
					;(vimpulse-imap (kbd "C-u") 'nflath-viper-scroll-down) 

					;(defun nflath-viper-scroll-up (&optional arg)
					;(interactive)
					;(dotimes (i 20)
					;(scroll-up 1)
					;(next-line)))
					;(vimpulse-map (kbd "C-d") 'nflath-viper-scroll-up)
					;(vimpulse-vmap (kbd "C-d") 'nflath-viper-scroll-up)
					;(vimpulse-imap (kbd "C-d") 'nflath-viper-scroll-up) 

;; Execute M-x using C-m
(vimpulse-map (kbd "\C-m") 'execute-extended-command)
(vimpulse-vmap (kbd "\C-m") 'execute-extended-command)
(vimpulse-imap (kbd "\C-m") 'execute-extended-command)

;; Execute find-file using C-f
(vimpulse-map (kbd "\C-f") 'find-file)
(vimpulse-vmap (kbd "\C-f") 'find-file)
(vimpulse-imap (kbd "\C-f") 'find-file)

;; C-w for backward-kill-word
(vimpulse-map (kbd "\C-w") 'backward-kill-word)
(vimpulse-vmap (kbd "\C-w") 'backward-kill-word)
(vimpulse-imap (kbd "\C-w") 'backward-kill-word)

;; Preserve kill-region
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; C-p and C-n for next-tab/prev-tab
; (vimpulse-map (kbd "\C-p") 'previous-tab-or-buffer)
; (vimpulse-vmap (kbd "\C-p") 'previous-tab-or-buffer)
; (vimpulse-imap (kbd "\C-p") 'previous-tab-or-buffer)
; (global-set-key "\C-p" 'previous-tab-or-buffer)

; (vimpulse-map (kbd "\C-n") 'next-tab-or-buffer)
; (vimpulse-vmap (kbd "\C-n") 'next-tab-or-buffer)
; (vimpulse-imap (kbd "\C-n") 'next-tab-or-buffer)
; (global-set-key "\C-n" 'next-tab-or-buffer)

;; Override location for configuration file, make it defualt to .emacs
(setq custom-file nil)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(aquamacs-additional-fontsets (quote (("apple" "monaco*" "medium" "r" "normal" 9 "monaco") ("apple" "monaco*" "medium" "r" "normal" 10 "monaco") ("apple" "monaco*" "medium" "r" "normal" 11 "monaco") ("apple" "monaco*" "medium" "r" "normal" 12 "monaco") ("apple" "monaco*" "medium" "r" "normal" 13 "monaco") ("apple" "monaco*" "medium" "r" "normal" 14 "monaco") ("apple" "monaco*" "medium" "r" "normal" 16 "monaco") ("apple" "monaco*" "medium" "r" "normal" 18 "monaco") ("apple" "lucida grande*" "medium" "r" "normal" 9 "lucida") ("apple" "lucida grande*" "medium" "r" "normal" 10 "lucida") ("apple" "lucida grande*" "medium" "r" "normal" 11 "lucida") ("apple" "lucida grande*" "medium" "r" "normal" 12 "lucida") ("apple" "lucida grande*" "medium" "r" "normal" 13 "lucida") ("apple" "lucida grande*" "medium" "r" "normal" 14 "lucida") ("apple" "lucida grande*" "medium" "r" "normal" 16 "lucida") ("apple" "lucida grande*" "medium" "r" "normal" 18 "lucida") ("apple" "lucida sans typewrite*" "medium" "r" "normal" 9 "lucida_typewriter") ("apple" "lucida sans typewrite*" "medium" "r" "normal" 10 "lucida_typewriter") ("apple" "lucida sans typewrite*" "medium" "r" "normal" 12 "lucida_typewriter") ("apple" "lucida sans typewrite*" "medium" "r" "normal" 14 "lucida_typewriter") ("apple" "lucida console*" "medium" "r" nil 11 "lucida_console") (nil "courier*" "medium" "r" nil 11 "courier") (nil "courier*" "medium" "r" nil 13 "courier") (nil "bitstream vera sans mono" "medium" "r" "normal" 10 "vera_mono") (nil "bitstream vera sans mono" "medium" "r" "normal" 12 "vera_mono") (nil "bitstream vera sans mono" "medium" "r" "normal" 14 "vera_mono"))) t)
 '(aquamacs-customization-version-id 190 t)
 '(aquamacs-tool-bar-user-customization nil t)
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
 '(show-paren-mode nil)
 '(transient-mark-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;; Always show line numbers
(global-linum-mode 1)

;; Auto-hide compile window
(defun notify-compilation-result(buffer msg)
  "Notify that the compilation is finished,
close the *compilation* buffer if the compilation is successful,
and set the focus back to Emacs frame"
  (if (string-match "^finished" msg)
      (progn
	(delete-windows-on buffer)
	(tooltip-show "\n Compilation Successful  \n "))
    (tooltip-show "\n Compilation Failed  \n "))
  (setq current-frame (car (car (cdr (current-frame-configuration)))))
  (select-frame-set-input-focus current-frame)
  )

(add-to-list 'compilation-finish-functions
	     'notify-compilation-result)

;; Support colors properly in shell using AnsiColor:
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Enable ssh somehow
(setq tramp-default-method "ssh")

;; Enable and configure ido-mode
(require 'ido)
(ido-mode t)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-enable-flex-matching t)
(ido-mode t)
(ido-everywhere t)
(setq ido-max-prospects 0)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq confirm-nonexistent-file-or-buffer nil)

