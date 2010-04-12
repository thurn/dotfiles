;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nathan Flath's save visited files modification, to reload buffers at start ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Reload files from last session
(require 'save-visited-files)
(setq save-visited-files-auto-restore t)
(setq save-visited-files-location (concat emacs-root-dthurn ".emacs-visited-files"))
(save-visited-files-mode t)
