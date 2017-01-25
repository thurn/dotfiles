;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The entry point into the magical world that is my emacs configurations.    ;;
;; This file defines some helper functions for loading other elisp files and  ;;
;; then loads all of the configuration directories.                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-directory-dthurn (dir)
  "Loads every .el file in a directory in sorted order"
  (mapcar 'load-file (directory-files dir t "\\.el\\'")))

(defun recursive-add-to-load-path-dthurn (dir)
  "Adds the supplied directory to the load-path, as well as an of its
   subdirectories"
  (let ((default-directory dir))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))

(setq user-emacs-directory "~/emacs/")

;; Set the load path
(if (not (getenv "TERM_PROGRAM"))
    (let ((path (shell-command-to-string
            "$SHELL -cl \"printf %s \\\"\\\$PATH\\\"\"")))
      (setenv "PATH" path)))
(setq exec-path (split-string (getenv "PATH") ":"))

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Set load path to be the site-lisp directory and all of its subdirectories.
;; This directory holds all of my emacs libraries.
(recursive-add-to-load-path-dthurn "~/emacs/lib")

;; Load customizations. NOTE: Sometimes, the order of customization loading is
;; important. To enable an ordering, the customizations are always loaded by
;; dthurn-load-directory above in alphabetical order as specified by lessp.
;; When customizations need to be ordered, prefix the file containing them
;; with aa to put them at the start of the load sequence or z to put them
;; at the end of the sequence, then differentiate among them using digits.
;; For example, to make visited.el load last, name it z1_visited.el
(load-directory-dthurn "~/emacs/conf")

(if (file-exists-p "~/.emacs_config.el")
  (load-file "~/.emacs_config.el"))

;; The final directory contains .el files that should be loaded as late as
;; possible in the startup process. The same rule about alphabetical loading
;; applies to final.
(load-directory-dthurn "~/emacs/final")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-process-type (quote ghci))
 '(haskell-stylish-on-save t)
 '(haskell-tags-on-save t)
 '(package-selected-packages
   (quote
    (flycheck-pos-tip flycheck-clojure parinfer dash rainbow-delimiters protobuf-mode magit csharp-mode company cider)))
 '(python-indent-offset 2)
 '(safe-local-variable-values
   (quote
    ((eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
           (add-hook
            (quote write-contents-functions)
            (lambda nil
              (delete-trailing-whitespace)
              nil))
           (require
            (quote whitespace))
           "Sometimes the mode needs to be toggled off and on."
           (whitespace-mode 0)
           (whitespace-mode 1))
     (whitespace-line-column . 80)
     (whitespace-style face trailing lines-tail)
     (require-final-newline . t)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1")))))
(put 'ido-exit-minibuffer 'disabled nil)
(put 'downcase-region 'disabled nil)
