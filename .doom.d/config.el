;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Derek Thurn")
 
(setq doom-font (font-spec :name "Iosevka" :size 14))

(setq doom-theme 'afternoon)

(setq display-line-numbers-type t)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq clojure-docstring-fill-column 78)

(after! smartparens
  (smartparens-global-mode -1)
  (smartparens-mode -1)
  (sp-pair "'" nil :actions :rem))

(after! company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-j") #'company-select-next)
  (define-key company-active-map (kbd "C-k") #'company-select-previous)
  (setq company-idle-delay 0.3))

(defun dthurn-license ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert
     (concat
      ";; Copyright © 2020-present Derek Thurn\n"
      ";;\n"
      ";; Licensed under the Apache License, Version 2.0 (the \"License\");\n"
      ";; you may not use this file except in compliance with the License.\n"
      ";; You may obtain a copy of the License at\n"
      ";;\n"
      ";; https://www.apache.org/licenses/LICENSE-2.0\n"
      ";;\n"
      ";; Unless required by applicable law or agreed to in writing, software\n"
      ";; distributed under the License is distributed on an \"AS IS\" BASIS,\n"
      ";; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.\n"
      ";; See the License for the specific language governing permissions and\n"
      ";; limitations under the License.\n"
      "\n"))))

(defun dthurn-page-down ()
  (interactive)
  (move-to-window-line nil)
  (forward-line 10)
  (scroll-up 10))

(defun dthurn-page-up ()
  (interactive)
  (move-to-window-line nil)
  (forward-line -10)
  (scroll-down 10))

(defun dthurn-previous-window ()
  (interactive)
  (other-window -1))

(defun dthurn-eval-buffer ()
  "Evaluate current buffer"
  (interactive)
  (save-buffer)
  (cond ((eq major-mode 'clojure-mode)
         (progn
           (call-interactively 'miracle-eval-namespace)
           (call-interactively 'miracle-eval-buffer)
           (switch-to-buffer-other-window "*miracle*")
           (other-window 1)))
        (t
         (call-interactively 'eval-buffer)))
  (message "Evaluated."))

(defun dthurn-close-all ()
  (interactive)
  (call-interactively '+popup/close-all))

(defun dthurn-switch-to-eshell ()
  (interactive)
  (select-window (window-at 0 0))
  (switch-to-buffer "*eshell*"))

(defun dthurn-switch-to-miracle ()
  (interactive)
  (select-window (window-at 0 0))
  (switch-to-buffer "*miracle*"))

(defun dthurn-format-buffer ()
  (interactive)
  (save-excursion
    (call-interactively 'parinfer-auto-fix)))

(defun dthurn-jump ()
  (interactive)
  (call-interactively 'history-add-history)
  (call-interactively 'lsp-find-definition))

(defun dthurn-back ()
  (interactive)
  (cond ((eq major-mode 'miracle-mode)
         (call-interactively 'comint-previous-input))
        ((eq major-mode 'eshell-mode)
         (call-interactively 'eshell-previous-input))
        ((eq major-mode 'clojure-mode)
         (progn
           (call-interactively 'history-prev-history)
           (call-interactively 'recenter)))))

(defun dthurn-forward ()
  (interactive)
  (cond ((eq major-mode 'miracle-mode)
         (call-interactively 'comint-next-input))
        ((eq major-mode 'eshell-mode)
         (call-interactively 'eshell-next-input))
        ((eq major-mode 'clojure-mode)
         (progn
           (call-interactively 'history-next-history)
           (call-interactively 'recenter)))))

(defun dthurn-miracle-describe ()
  "Ask user about symbol and show symbol documentation if found."
  (interactive
   (let* ((sym (thing-at-point 'symbol))
          (sym (if sym (substring-no-properties sym))))
     (miracle-eval-doc sym))))

(defun dthurn-doc ()
  (interactive)
  (call-interactively 'dthurn-miracle-describe))

(defun dthurn-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun dthurn-populate ()
  (interactive)
  (call-interactively 'miracle-eval-namespace)
  (call-interactively 'miracle-eval-buffer)
  (switch-to-buffer-other-window "*miracle*")
  (insert
   (concat "(let [n (ns-name *ns*)]"
           "(require '[nighthollow.repl])"
           "(@(resolve (symbol \"nighthollow.repl\" \"populate\")))"
           "(in-ns n))"))
  (call-interactively' comint-send-input)
  (other-window 1))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn  (rename-file name new-name 1)
                (rename-buffer new-name)
                (set-visited-file-name new-name)
                (set-buffer-modified-p nil))))))

(defun dthurn-beginning-of-buffer ()
  (interactive)
  (call-interactively 'beginning-of-buffer))

(map!
 "C-q" #'recenter
 "C-w" #'dthurn-page-up
 "C-r" #'backward-kill-word
 "C-t" #'other-window
 "C-y" #'goto-line
 "C-u" #'yank
 "M-\\" #'dthurn-page-down ; Remapped from C-i
 "s-\\" #'dthurn-page-down ; Remapped from C-i
 "C-o" #'counsel-find-file
 "C-p" #'dthurn-previous-window

 "C-z" #'dthurn-forward
 "C-v" #'save-buffer
 "C-b" #'dthurn-back
 "C-n" #'kill-line
 "M-y" #'delete-char ; Remapped from C-m
 "s-y" #'delete-char ; Remapped from C-m
 "C-," #'counsel-M-x

 "s-w" #'dthurn-close-all
 "s-e" #'dthurn-format-buffer
 "s-u" #'dthurn-beginning-of-buffer
 "s-i" #'dthurn-eval-buffer
 "s-o" #'imenu-anywhere
 "s-p" #'counsel-imenu

 "s-a" #'mark-whole-buffer
 "s-s" #'delete-other-windows
 "s-d" #'kill-word
 "s-g" #'end-of-buffer
 "s-h" #'dthurn-doc
 "s-j" #'counsel-switch-buffer
 "s-k" #'set-mark-command
 "s-l" #'isearch-backward
 "s-;" #'eval-expression
 "s-'" #'dthurn-switch-to-eshell
 "s-[" #'dthurn-switch-to-miracle

 "s-x" #'kill-region
 "s-c" #'copy-region-as-kill
 "s-b" #'comint-previous-matching-input-from-input
 "s-m" #'dthurn-previous-buffer
 "s-." #'dthurn-jump

 "s-`" #'other-window

 "s-I" #'dthurn-populate

 "M-l" #'dthurn-license)

(bind-keys*
 ("C-j" . next-line)
 ("C-k" . previous-line)
 ("C-d" . forward-char)
 ("C-f" . forward-word)
 ("C-l" . backward-word)
 ("C-;" . backward-char)
 ("C-'" . join-line))
