;; Some useful elisp functions
(require 'cl)

;; Split the screen into 80-column windows
(defun smart-split ()
  "Split the frame into 80-column sub-windows, and make sure no window has
   fewer than 80 columns."
  (interactive)
  (defun smart-split-helper (w)
    "Helper function to split a given window into two, the first of which has
     80 columns."
    (if (> (window-width w) (* 2 81))
        (let ((w2 (split-window w 82 t)))
          (smart-split-helper w2))))
  (smart-split-helper nil))

;; Function to indent the whole buffer
(defun iwb-dthurn ()
  "Indents the entire buffer"
  (interactive)
  (indent-region (point-min) (point-max) nil))

;; Function to remove tabs in the whole buffer
(defun uwb-dthurn ()
  "Untabifies the whole buffer"
  (interactive)
  (untabify (point-min) (point-max)))

(defun dlog (&rest objects)
  "Logs output to a buffer called *log*"
  (lexical-let* ((buffer (get-buffer-create "*log*")))
    (with-current-buffer buffer
      (end-of-buffer)
      (insert (concat "[" (current-time-string) "] "))
      (mapcar (lambda (obj) (princ obj buffer) (insert " ")) objects)
      (newline))))

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

(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dirs (if (string-match dir "\\(?:/\\|\\\\)$")
                   (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))

    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn (copy-file filename newname 1)
             (delete-file filename)
             (set-visited-file-name newname)
             (set-buffer-modified-p nil) t))))
(fset 'hlog
      "\C-ahlog(';;; \C-n\C-u', print_shallow(\C-u));")

(fset 'python-log
      "\C-aprint '\C-n\C-u: ', str(\C-u)")

(defun shell-and-cd (&rest args)
  (interactive)
  (let ((dir default-directory))
    (switch-to-buffer "*shell*")
    (kill-new (concat "cd " dir) nil)))

(defun color-buffer ()
  "Removes ANSI escape characters from the buffer and attempts to color it"
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))


