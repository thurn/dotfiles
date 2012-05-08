;(defvar sync-remote-host "hostname")
;(defvar sync-remote-dir "remote")
;(defvar sync-local-dirs '("local"))

(defun starts-with? (string substr)
  "Check if string starts with substr"
  (if (< (length string) (length substr))
      nil
    (if (equal substr
               (substring string 0 (length substr))) 't nil)))

(defun async-save ()
  (loop for sync-local-dir in sync-local-dirs do
        (if (starts-with? buffer-file-name sync-local-dir)
            (save-window-excursion
              (let ((buffer (generate-new-buffer "sync")))
                (async-shell-command
                 (concat "scp " buffer-file-name " " sync-remote-host ":"
                         sync-remote-dir
                         (substring
                          buffer-file-name
                          (length sync-local-dir)))
                 buffer)
                (run-with-timer
                 60 nil
                 (lambda (buffer)
                   (kill-buffer-if-exists buffer))
                 buffer))))))



;(add-hook 
; 'after-save-hook 
; 'async-save)


