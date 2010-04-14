;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations that are not specific to any particular Emacs module.       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Inhibit the creation of new frames.
(setq pop-up-frames nil)

;; Cause Emacs to use the box cursor
(setq-default cursor-type 'box)

;; Always show line numbers
;; (global-linum-mode 1)

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

(defun indent-whole-buffer-dthurn ()
  "Indents the entire buffer"
  (interactive)
  (indent-region (point-min) (point-max) nil))
