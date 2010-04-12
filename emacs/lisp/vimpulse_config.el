;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration for vimpulse, the vim emulation package.                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load vimpulse
(require 'vimpulse)

(defun normal-map-dthurn (str fun)
  "Maps a key combination to a function in both normal and visual mode"
  (vimpulse-map (read-kbd-macro str) fun)
  (vimpulse-vmap (read-kbd-macro str) fun))

(defun all-map-dthurn (str fun)
  "Maps a key combination to a function in normal, visual, and insert modes"
  (vimpulse-imap (read-kbd-macro str) fun)
  (global-set-key (read-kbd-macro str) fun)
  (normal-map-dthurn str fun))

;; Change the key for escape
(all-map-dthurn "C-;" 'viper-intercept-ESC-key)

;; Swap ; and :
(normal-map-dthurn ";" 'viper-ex)
(normal-map-dthurn ":" 'viper-repeat-find)

;; Map s to save and C-s to s
(normal-map-dthurn "s" 'save-buffer)
(normal-map-dthurn "C-s" 'viper-substitute)

;; H to go to start of line, L to go to end of line
(normal-map-dthurn "H" 'viper-bol-and-skip-white)
(normal-map-dthurn "L" 'viper-goto-eol)

;; Scrolling keeps the point in the same place.
(setq scroll-preserve-screen-position t)

;; Fix for Vimpulse Ctrl-u scrolling
(all-map-dthurn "C-u" 'viper-scroll-down)

;; Switch buffers
(all-map-dthurn "C-b" 'ido-switch-buffer)
(all-map-dthurn "C-x C-b" 'ido-switch-buffer)

;; Execute find-file
(all-map-dthurn "C-f" 'ido-find-file)

;; Map C-x f to open files... I keep hitting this by mistake!
(all-map-dthurn "C-x f" 'ido-find-file)

;; Mapping for backward-kill-word
(all-map-dthurn "C-w" 'backward-kill-word)

;; Execute M-x commands
(all-map-dthurn "C-u" 'execute-extended-command)

;; Perl regular expression find/replace
(vimpulse-map (kbd "gs")
              (lambda ()
                (interactive)
                (shell-command-on-region (point-min)
                                         (point-max)
                                         (concat "perl -p -i -e 's/"
                                                 (read-string ":%!perl -p -i -e 's/"))
                                         nil
                                         t)))

