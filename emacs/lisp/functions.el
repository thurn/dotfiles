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

