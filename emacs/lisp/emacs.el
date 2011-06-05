;; Customizations that are not specific to any particular Emacs module.

;; Inhibit the creation of new frames.
(setq pop-up-frames nil)

;; Cause Emacs to use the box cursor
(setq-default cursor-type 'box)

;; Automatically pull changes to open buffers
(global-auto-revert-mode t)

;; Use spaces, not tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Don't use the Emacs GUI
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Use qrr as an alias for query-replace-regex
(defalias 'qrr 'query-replace-regexp)

;; Define a backup directory
(setq backup-directory-alist `(("." . "~/emacs/backups/")))

;; Define the emacs font
(setq dthurn-font-name "Inconsolata-dz")
(if (member dthurn-font-name (font-family-list))
    (set-face-attribute 'default nil :font "Inconsolata-dz" :height 120))

;; Don't show startup screen
(defun display-startup-screen (&optional concise))

;; Fix org-mode bug
(setq calendar-mode-map '(keymap nil))

;; Maximize vertical space
(setq split-height-threshold nil)
