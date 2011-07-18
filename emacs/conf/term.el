;; Configuration for the emacs shell and the emacs terminal

(defvar shell-path-dthurn (executable-find "bash")
  "Shell location")

;; Support colors properly in shell using AnsiColor:
(require 'term)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq comint-prompt-read-only t)
(add-hook 'shell-mode-hook (lambda ()
                             (setq show-trailing-whitespace nil)
                             (highlight-80+-mode -1)))


(defun ansi-term-current-directory ()
  "Opens a ansi-term in the current directory"
  (interactive)
  (let ((new-buffer-name
         (concat "term-"
                 (expand-file-name default-directory) "-term" )))
    (if
        (get-buffer (concat "*" new-buffer-name "*"))
        (switch-to-buffer (concat "*" new-buffer-name "*" ))
      (ansi-term shell-path-dthurn new-buffer-name))))

;; Use C-x as the escape character in *term* so many Emacs commands work
;; naturally
(term-set-escape-char ?\C-x)

;; Use bash in emacs, it works better
(setq shell-file-name "/bin/bash")
(setq explicit-shell-file-name "/bin/bash")

