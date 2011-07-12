
(defvar sync-remote-host "dev2181.snc6.facebook.com")
(defvar sync-remote-dir "/home/dthurn/www")
(defvar sync-local-dir "/Users/dthurn/www")

(defun starts-with? (string substr)
  "Check if string starts with substr"
  (if (< (length string) (length substr))
      nil
    (if (equal substr
               (substring string 0 (length substr))) 't nil)))

(defun async-save ()
  (if (starts-with? buffer-file-name sync-local-dir)
      (save-window-excursion
        (shell-command-to-string
         (concat "scp " buffer-file-name " " sync-remote-host ":"
                 sync-remote-dir
                 (substring
                  buffer-file-name
                  (length sync-local-dir)) " &")))))

(add-hook 
 'after-save-hook 
 'async-save)


