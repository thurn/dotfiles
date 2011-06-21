;; Configuration for Dired+

(require 'dired+)

(toggle-dired-find-file-reuse-dir 1)
(define-key dired-mode-map (kbd "<DEL>") 'dired-up-directory)


;; The above will still create a new buffer if you invoke ^ (dired-up-directory)
;; This fixed that
(eval-after-load "dired"
  ;; don't remove 'other-window', the caller expects it to be there
  '(defun dired-up-directory (&optional other-window)
     "Run Dired on parent directory of current directory."
     (interactive "P")
     (let* ((dir (dired-current-directory))
            (orig (current-buffer))
            (up (file-name-directory (directory-file-name dir))))
       (or (dired-goto-file (directory-file-name dir))
           ;; Only try dired-goto-subdir if buffer has more than one dir.
           (and (cdr dired-subdir-alist)
                (dired-goto-subdir up))
           (progn
             (kill-buffer orig)
             (dired up)
             (dired-goto-file dir))))))