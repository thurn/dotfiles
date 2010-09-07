;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations that are not specific to any particular Emacs module.       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Inhibit the creation of new frames.
(setq pop-up-frames nil)

;; Cause Emacs to use the box cursor
(setq-default cursor-type 'box)

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

;; Function to indent the whole buffer
(defun iwb-dthurn ()
  "Indents the entire buffer"
  (interactive)
  (indent-region (point-min) (point-max) nil))

;; Automatically pull changes to open buffers
(global-auto-revert-mode t)

;; Use spaces, not tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Function to remove tabs in the whole buffer
(defun uwb-dthurn ()
 "Untabifies the whole buffer"
 (interactive)
 (untabify (point-min) (point-max)))

;; Don't use the Emacs GUI
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Use qrr as an alias for query-replace-regex
(defalias 'qrr 'query-replace-regexp)

;; Define a backup directory
(setq backup-directory-alist `(("." . "~/emacs/backups/")))

;; Define the emacs font
;; (set-face-attribute 'default nil :font "Anonymous Pro")

