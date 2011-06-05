;; Configuration for the emacs shell and the emacs terminal

(defvar shell-path-dthurn (executable-find "zsh")
     "Shell location")

;; Support colors properly in shell using AnsiColor:
(require 'term)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq comint-prompt-read-only t)

(defun ansi-term-current-directory ()
  "Opens a ansi-term in the current directory"
  (interactive)
  (let ((new-buffer-name (concat "term-" (expand-file-name default-directory) "-term" )))
    (if (get-buffer (concat "*" new-buffer-name "*")) (switch-to-buffer (concat "*" new-buffer-name "*" ))
      (ansi-term shell-path-dthurn new-buffer-name))))

;; Use C-x as the escape character in *term* so many Emacs commands work naturally
(term-set-escape-char ?\C-x)
