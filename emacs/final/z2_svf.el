;; Configuration for save-visited-files mode

(require 'save-visited-files)

;; TODO(dthurn): Figure out how to stop this hook from getting set at all.
(remove-hook 'find-file-hook 'vc-find-file-hook)

(setq save-visited-files-auto-restore t)
(setq save-visited-files-location "~/.emacs-visited-files")
(save-visited-files-mode t)
