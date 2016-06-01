;; Customizations that are not specific to any particular Emacs module.

;; Inhibit the creation of new frames.
(setq pop-up-frames nil)

;; Cause Emacs to use the box cursor
(setq-default cursor-type 'box)

;; Automatically pull changes to open buffers
(global-auto-revert-mode t)

;; Use spaces, not tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Don't use the Emacs GUI
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Use qrr as an alias for query-replace-regex
(defalias 'qrr 'query-replace-regexp)

;; Define a backup directory
(setq dthurn-backup-file-directory "~/emacs/backups")
(setq backup-directory-alist
      `((".*" . ,dthurn-backup-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,dthurn-backup-file-directory t)))
(setq backup-by-copying t)

;; Define the emacs font
(if (member "Inconsolata-dz" (font-family-list))
    (set-face-attribute 'default nil :font "Inconsolata-dz" :height 120))

;; Don't show startup screen
(defun display-startup-screen (&optional concise))

;; Fix org-mode bug
(setq calendar-mode-map '(keymap nil))

;; Maximize vertical space
(setq split-height-threshold nil)

;; Set my local path
(setenv "PATH"
  (concat
   (getenv "HOME")
   "/bin:"
   (getenv "HOME")
   "/haskell/bin:"
   "/usr/local/bin:"
   (getenv "HOME")
   "/.cabal/bin:"
   (getenv "PATH")))

;; Set exec-path based on $PATH
(setq exec-path (split-string (getenv "PATH") ":"))
(delete-dups exec-path)

;; Set eshell-path-env based on exec-path
(setq eshell-path-env (mapconcat 'identity exec-path ":"))

;; Use emacs for $EDITOR
(setenv "EDITOR" (concat (getenv "HOME") "/bin/emacs_wrapper.rb"))

;; Hack around programs that don't think eshell is a fully-functional terminaldd
(setenv "TERM" "xterm")

;; Uniquify buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Enable the mouse over xterm
(xterm-mouse-mode t)

;; Show column number
(column-number-mode t)

;; These default VC functions are incredibly slow under OSX (> 5 seconds) for me.
;; Disabling them for now.
(defun vc-find-file-hook () nil)
(defun vc-before-save () nil)
(defun vc-after-save () nil)

;; Kill trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(set-default 'fill-column 80)

;; Start the server
(server-start)

(setq-default fill-column 80)

;; Disable the bell
(setq ring-bell-function 'ignore)
