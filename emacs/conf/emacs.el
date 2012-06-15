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
(setq backup-directory-alist `(("." . "~/emacs/backups/")))

;; Define the emacs font
(setq dthurn-font-name "Monaco")
(if (member dthurn-font-name (font-family-list))
    (set-face-attribute 'default nil :font "Monaco" :height 120))

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
   (getenv "HOME")
   "/.cabal/bin:"
   (getenv "PATH")))

(setq exec-path (split-string (getenv "PATH") ":"))

;; Use emacs for $EDITOR
(setenv "EDITOR" (concat (getenv "HOME") "/bin/emacs_wrapper.rb"))

;; Hack around programs that don't think eshell is a fully-functional terminal
(setenv "TERM" "xterm")

;; Uniquify buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Always run a server
(server-start)