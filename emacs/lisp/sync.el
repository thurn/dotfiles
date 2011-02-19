(defvar sync-remote-host "uvm")
(defvar sync-remote-dir "/home/dthurn/Documents")
(defvar sync-local-dir "/Users/dthurn/Documents/uvm")

(defun starts-with? (string substr)
  "Check if string starts with substr"
  (if (< (length string) (length substr))
      nil
      (if (equal substr
                 (substring string 0 (length substr))) 't nil)))

(add-hook 
  'after-save-hook 
  (lambda ()
    (if (starts-with? buffer-file-name sync-local-dir)
        (shell-command
         (concat "scp " buffer-file-name " " sync-remote-host ":"
                 sync-remote-dir (substring
                                  buffer-file-name (length sync-local-dir)))))))

