;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations that are not specific to any particular Emacs module.       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Inhibit the creation of new frames.
(setq pop-up-frames nil)

;; Cause Emacs to use the box cursor
(setq-default cursor-type 'bar)

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

;; Automatically pull changes to open buffers
(global-auto-revert-mode t)

;; Use spaces, not tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Don't use the Emacs GUI
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Use qrr as an alias for query-replace-regex
(defalias 'qrr 'query-replace-regexp)

;; Define a backup directory
(setq backup-directory-alist `(("." . "~/emacs/backups/")))

;; Define the emacs font
;; (set-face-attribute 'default nil :font "Anonymous Pro")

;; Don't show startup screen
(defun display-startup-screen (&optional concise))

(defvar programming-major-mode-hooks
 '(emacs-lisp-mode-hook
   lisp-interaction-mode-hook
   inferior-emacs-lisp-mode-hook
   scheme-mode-hook
   sql-mode-hode
   lisp-mode-hook
   haskell-mode-hook
   clojure-mode-hook
   python-mode-hook
   c-mode-common-hook
   php-mode-hook
   )
 "List of programming mode hooks.")

(defun add-hook-to-all (hooks fn)
 "Add a function to a list of hooks."
 (mapcar (lambda (hook) (add-hook hook fn)) hooks))

(calendar)
