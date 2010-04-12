;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration for vimpulse, the vim emulation package.                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load vimpulse
(require 'vimpulse)

(defun dthurn-normal-map (str fun)
  "Maps a key combination to a function in both normal and visual mode"
  (vimpulse-map (read-kbd-macro str) fun)
  (vimpulse-vmap (read-kbd-macro str) fun))

(defun dthurn-all-map (str fun)
  "Maps a key combination to a function in normal, visual, and insert modes"
  (vimpulse-imap (read-kbd-macro str) fun)
  (dthurn-normal-map str fun))

;; Map Ctrl-K to Escape
(dthurn-all-map "C-k" 'viper-intercept-ESC-key)

;; Swap ; and :
(dthurn-normal-map ";" 'viper-ex)
(dthurn-normal-map ":" 'viper-repeat-find)

;; Map s to save and C-s to s
(dthurn-normal-map "s" 'save-buffer)
(dthurn-normal-map "C-s" 'viper-substitute)

;; H to go to start of line, L to go to end of line
(dthurn-normal-map "H" 'viper-bol-and-skip-white)
(dthurn-normal-map "L" 'viper-goto-eol)

;; Scrolling keeps the point in the same place.
(setq scroll-preserve-screen-position t)

;; Fix for Vimpulse Ctrl-u scrolling
(dthurn-all-map "C-u" 'viper-scroll-down)

;; Ctrl-b to switch buffers
(dthurn-all-map "C-b" 'ido-switch-buffer)

;; Execute find-file using Ctrl-f
(dthurn-all-map "C-f" 'ido-find-file)

;; C-w for backward-kill-word
(dthurn-all-map "C-w" 'backward-kill-word)

;; Execute M-x using C-m
(dthurn-all-map "C-m" 'execute-extended-command)

;; gs to open a perl substitute command
(vimpulse-map (kbd "gs")
              (lambda ()
                (interactive)
                (shell-command-on-region (point-min)
                                         (point-max)
                                         (concat "perl -p -i -e 's/"
                                                 (read-string ":%!perl -p -i -e 's/"))
                                         nil
                                         t)))

