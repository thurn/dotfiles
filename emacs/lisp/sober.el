;; Provides Sober-Mode, Giving You Sober Keybindings As A Minor Mode

;; Function to cycle between hard and soft bol
(defun dthurn-cycle-bol (&optional arg)
  "If at the first non-whitespace character of a line, go to the
beginning of the current line.  otherwise, goto the first non-whitespace
character of the current line."
  (interactive)
  (cond
   ((bolp) (back-to-indentation))
   ((save-excursion
      (let ((pt (point)))
        (back-to-indentation)
        (eq pt (point)))) (beginning-of-line))
   (t (back-to-indentation))))

;; Functions that search history instead of using up/down in shell/slime mode
(defun dthurn-up (&rest args)
  (interactive)
  (cond ((eq major-mode 'shell-mode)
         (call-interactively 'comint-previous-input))
        ((eq major-mode 'slime-repl-mode)
         (call-interactively 'slime-repl-backward-input))
        (t
         (call-interactively 'previous-line))))

(defun dthurn-down (&rest args)
  (interactive)
  (cond ((eq major-mode 'shell-mode)
         (call-interactively 'comint-next-input))
        ((eq major-mode 'slime-repl-mode)
         (call-interactively 'slime-repl-forward-input))
        (t
         (call-interactively 'next-line))))

(defun dthurn-open (&rest args)
  (interactive)
  (if (eq major-mode 'shell-mode)
      (call-interactively 'ido-find-file-other-window)
    (call-interactively 'ido-find-file)))

(defun dthurn-bol (&rest args)
  (interactive)
  (call-interactively 'dthurn-cycle-bol)
  (if (eq major-mode 'slime-repl-mode) 
      (call-interactively 'forward-word)))

(defun dthurn-compile (&rest args)
  (interactive)
  (call-interactively 'iwb-dthurn)
  (call-interactively 'save-buffer)
  (call-interactively 'slime-compile-and-load-file)
  (slime-repl-set-package (slime-pretty-package-name (slime-current-package)))
  (call-interactively 'slime-switch-to-output-buffer))

(defun dthurn-goto-symbol (&rest args)
  (interactive)
  (call-interactively 'ido-goto-symbol)
  (call-interactively 'recenter))

(defun dthurn-page-down (&rest args)
  (interactive)
  (dotimes (_ 5 _)
    (next-line)
    (scroll-up 1)))

(defun dthurn-page-up (&rest args)
  (interactive)
  (dotimes (_ 5 _)
    (previous-line)
    (scroll-down 1)))

(defvar sober-mode-map (make-keymap)
   "Keymap for sober-mode.")

(defmacro sober-map-key (key command)
  `(global-set-key (kbd ,key) ,command)
  `(define-key sober-mode-map (kbd ,key) ,command))

(defmacro sober-define (key name body)
  `(defun ,name (&rest args)
      (interactive)
      ,body))

;; Global keybindings

;; ; Top Row
;; (sober-define "C-q" sober-cq (recenter))
;; (sober-define "C-w" sober-cw (dthurn-page-up))
;; (sober-define "C-e" sober-ce (end-of-line))
;; (sober-define "C-r" sober-cr (backward-kill-word))
;; (sober-define "C-t" sober-ct (other-window))
;; (sober-define "C-y" sober-cy (goto-line))
;; (sober-define "C-u" sober-cu (yank))
;; (sober-define "M-t" sober-ci (dthurn-page-down))
;; (sober-define "C-o" sober-co (dthurn-open))
;; (sober-define "C-p" sober-cp (other-previous-window))

;; ; Middle Row
;; (sober-define "C-a" sober-ca (dthurn-bol))
;; (sober-define "C-d" sober-cd (forward-char))
;; (sober-define "C-f" sober-cf (forward-word))
;; (sober-define "C-g" sober-cg (keyboard-escape-quit))
;; (sober-define "C-j" sober-cj (dthurn-down))
;; (sober-define "C-k" sober-ck (dthurn-up))
;; (sober-define "C-l" sober-cl (backward-word))
;; (sober-define "C-;" sober-csemi (backward-char))

;; ; Bottom Row
;; ; C-z is 'alternate down', not set yet
;; (sober-define "C-v" sober-cv (save-buffer))
;; ; C-b is 'alternate up', not set yet
;; (sober-define "C-n" sober-cn (kill-line))
;; (sober-define "M-y" sober-cm (delete-char))
;; (sober-define "C-," sober-ccomma (smex))
;; (sober-define "C-/" sober-cslash (undo))

;;; Global keybindings

;; ; Top Row (Meta)
;; (sober-map-key "M-q" 'sober-mq)
;; (sober-map-key "M-w" 'sober-mw)
;; (sober-map-key "M-e" 'sober-me) ; Override as a formatting command
;; (sober-map-key "M-r" 'sober-mr)
;; (sober-map-key "M-u" 'sober-mu)
;; (sober-map-key "M-i" 'sober-mi) ; Override as a compile command
;; (sober-map-key "M-p" 'sober-mp)

;; ; Middle Row
;; (sober-map-key "M-a" 'sober-ma)
;; (sober-map-key "M-s" 'sober-ms)
;; (sober-map-key "M-d" 'sober-md)
;; (sober-map-key "M-f" 'sober-mf)
;; (sober-map-key "M-g" 'sober-mg)
;; (sober-map-key "M-h" 'sober-mh)
;; (sober-map-key "M-j" 'sober-mj)
;; (sober-map-key "M-k" 'sober-mk)
;; (sober-map-key "M-l" 'sober-ml)
;; (sober-map-key "M-;" 'sober-msemi)

;; ; Bottom Row
;; (sober-map-key "M-z" 'sober-mz)
;; (sober-map-key "M-x" 'sober-mx)
;; (sober-map-key "M-c" 'sober-mc)
;; (sober-map-key "M-v" 'sober-mv)
;; (sober-map-key "M-n" 'sober-mn)
;; (sober-map-key "M-." 'sober-mdot)
;; (sober-map-key "M-/" 'sober-mslash)
;; (sober-map-key "M-`" 'sober-mbtick)

; Top Row
(sober-map-key "C-q" 'recenter)
(sober-map-key "C-w" 'dthurn-page-up)
(sober-map-key "C-e" 'end-of-line)
(sober-map-key "C-r" 'backward-kill-word)
(sober-map-key "C-t" 'other-window)
(sober-map-key "C-y" 'goto-line)
(sober-map-key "C-u" 'yank)
(sober-map-key "M-t" 'dthurn-page-down) ; REMAPPED AT OS LEVEL TO SEND C-i
(sober-map-key "C-o" 'dthurn-open)
(sober-map-key "C-p" (lambda () (interactive) (other-window -1)))

; Middle Row
(sober-map-key "C-a" 'dthurn-bol)
(sober-map-key "C-d" 'forward-char)
(sober-map-key "C-f" 'forward-word)
(sober-map-key "C-g" 'keyboard-escape-quit)
(sober-map-key "C-j" 'dthurn-down)
(sober-map-key "C-k" 'dthurn-up)
(sober-map-key "C-l" 'backward-word)
(sober-map-key "C-;" 'backward-char)

; Bottom Row
; C-z is 'alternate down', not set yet
(sober-map-key "C-v" 'save-buffer)
; C-b is 'alternate up', not set yet
(sober-map-key "C-n" 'kill-line)
(sober-map-key "M-y" 'delete-char) ; REMAPPED AT OS LEVEL TO SEND C-m
(sober-map-key "C-," 'smex)
(sober-map-key "M-[ a" 'smex) ; REMAPPED AT OS LEVEL TO SEND C-, in TERMINAL
(sober-map-key "C-/" 'undo)

; Top Row (Meta)
(sober-map-key "M-q" 'save-buffers-kill-terminal)
(sober-map-key "M-w" 'delete-frame)
(sober-map-key "M-e" 'iwb-dthurn) ; Override as a formatting command
(sober-map-key "M-r" 'forward-paragraph)
(sober-map-key "M-u" 'beginning-of-buffer)
(sober-map-key "M-i" 'eval-buffer) ; Override as a compile command
(sober-map-key "M-p" 'ido-goto-symbol)

; Middle Row
(sober-map-key "M-a" 'mark-whole-buffer)
(sober-map-key "M-s" 'delete-other-windows)
(sober-map-key "M-d" 'kill-word)
(sober-map-key "M-f" 'forward-sexp)
(sober-map-key "M-g" 'end-of-buffer)
(sober-map-key "M-h" 'backward-sexp)
(sober-map-key "M-j" 'ido-switch-buffer)
(sober-map-key "M-k" 'set-mark-command)
(sober-map-key "M-l" 'isearch-backward)
(sober-map-key "M-;" 'doctor)

; Bottom Row
(sober-map-key "M-z" 'undo)
(sober-map-key "M-x" 'kill-region)
(sober-map-key "M-c" 'copy-region-as-kill)
(sober-map-key "M-v" 'clipboard-yank)
(sober-map-key "M-n" 'backward-paragraph)
(sober-map-key "M-." 'find-tag)
(sober-map-key "M-/" 'comment-or-uncomment-region-or-line)
(sober-map-key "M-`" 'other-window)

; (global-set-key (kbd "C-s") (lambda () (interactive) (recenter)))

;;;###autoload
 (define-minor-mode sober-mode
   "Minor mode to enable the sober keybinding system."
   :init-value nil
   :group 'sober)

;;;###autoload
(defun turn-on-sober-mode ()
  "Turns on sober mode if the buffer is appropriate."
  (sober-mode t))

(define-global-minor-mode sober-global-mode sober-mode turn-on-sober-mode)

(sober-global-mode t)
