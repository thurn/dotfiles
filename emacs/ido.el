;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration for ido mode, to Interactively DO things with buffers.       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ido)

;; Enable and configure ido-mode
(ido-mode t)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-enable-flex-matching t)
(ido-mode t)
(ido-everywhere t)
(setq ido-max-prospects 0)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq confirm-nonexistent-file-or-buffer nil)
