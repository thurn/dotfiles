;; Configuration for the emacs shell and the emacs terminal

(defvar shell-path-dthurn (executable-find "bash")
  "Shell location")

;; Support colors properly in shell using AnsiColor:
(require 'term)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq comint-prompt-read-only t)
(add-hook 'shell-mode-hook (lambda ()
                             (setq show-trailing-whitespace nil)))


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

(defun dthurn-cleanup-prompt (path)
  (replace-regexp-in-string (getenv "HOME") "~" path))

(setq eshell-prompt-function
      (lambda ()
        (concat
         (dthurn-cleanup-prompt (eshell/pwd)) "\n$ ")))

(setq eshell-prompt-regexp "^[^$]*[$][ ]")

(defun eshell/emacs (file)
  (find-file-other-window file))

(defun eshell/vim (file)
  (find-file-other-window file))

(defun eshell/lock ()
  (shell-command "gnome-screensaver-command --lock"))

(defun eshell/logoff ()
  (shell-command "gnome-session-save --logout"))

(defun eshell/reboot ()
  (shell-command "sudo shutdown -r now"))

(defun dthurn-cmdjoin (list)
  (if list (concat "'" (mapconcat 'identity list "' '") "'") ""))

(defun dthurn-async-git-exec (command args)
  (async-shell-command (concat "git " command " " (dthurn-cmdjoin args))))

(defun dthurn-git-exec (command args)
  (shell-command (concat "git " command " " (dthurn-cmdjoin args))))

(defun eshell/ll ()
  (eshell/ls "-l" "-a" "-h"))

(defun eshell/ack (pattern)
  (ack pattern nil default-directory)
  (switch-to-buffer-other-window "*Ack-and-a-half*"))

(defun eshell/ackr (pattern)
  (ack pattern t default-directory)
  (switch-to-buffer-other-window "*Ack-and-a-half*"))

(defun eshell/git (command &rest args)
  (cond
   ((equal command "log")
    (let ((inhibit-read-only t))
      (shell-command "git log -n 100 --color")
      (switch-to-buffer-other-window "*Shell Command Output*")
      (color-buffer)
      (help-mode)))
   ((equal command "diff")
    (let ((inhibit-read-only t))
      (shell-command "git diff --color")
      (switch-to-buffer-other-window "*Shell Command Output*")
      (color-buffer)
      (help-mode)))
   ((member command '("rebase" "grep" "clone"))
    (dthurn-async-git-exec command args))
   ((member command '("branch" "mv" "stash" "commit" "add" "tag" "reset" "help"
                      "merge" "rm" "push" "status" "clean" "mergetool" "config"
                      "pull" "push" "checkout" "remote" "fetch" "init"))
    (dthurn-git-exec command args))
   (t (concat "Command not supported: " command))))
