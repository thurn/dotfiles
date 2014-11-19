;; Provides Sober-Mode, Giving You Sober Keybindings As A Minor Mode
(require 'cl)

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
  (if (or (eq major-mode 'eshell-mode) (eq major-mode 'shell-mode))
      (call-interactively 'ido-find-file-other-window)
    (call-interactively 'ido-find-file)))

(defun dthurn-bol (&rest args)
  (interactive)
  (cond ((eq major-mode 'eshell-mode)
         (call-interactively 'eshell-bol))
        ((eq major-mode 'cider-repl-mode)
          (call-interactively 'cider-repl-bol))
        ((eq major-mode 'haskell-interactive-mode)
         (call-interactively 'haskell-interactive-mode-beginning))
        (t
         (call-interactively 'dthurn-cycle-bol))))

(defun dthurn-compile (&rest args)
  (interactive)
  (call-interactively 'iwb-dthurn)
  (call-interactively 'save-buffer)
  (call-interactively 'slime-compile-and-load-file)
  (slime-repl-set-package (slime-pretty-package-name (slime-current-package)))
  (call-interactively 'slime-switch-to-output-buffer))

(defun dthurn-goto-symbol (&rest args)
  (interactive)
  (call-interactively 'textmate-goto-symbol)
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

(defun dthurn-comment-or-uncomment-region-or-line ()
  "Like comment-or-uncomment-region, but if there's no mark (that means no
region) apply comment-or-uncomment to the current line"
  (interactive)
  (if (not mark-active)
      (comment-or-uncomment-region
       (line-beginning-position) (line-end-position))
    (if (< (point) (mark))
        (comment-or-uncomment-region (point) (mark))
      (comment-or-uncomment-region (mark) (point)))))

(defun kill-buffer-if-exists (name)
  "Kill a buffer named 'name' if it exists"
  (if (not (eq nil (get-buffer name)))
      (kill-buffer  name)))

(defun sober-map-ci (command)
  """Maps C-i to a specific command via some trickery."""
  ;; Don't translate tab into C-i.
  (define-key function-key-map [tab] nil)
  ;; Swap the meanings of tab and C-i.
  (define-key key-translation-map [9] [tab])
  (define-key key-translation-map [tab] [9])
  ;; Bind tab (which is now actually C-i)
  (global-set-key [tab] 'comand))

(defun dthurn-kill-starred-buffers ()
  "Kill some buffers that annoy me"
  (interactive)
  (let ((buffers
         '("*Help*" "*Apropos*" "*Completions*" "*JDEE bsh*" "*Backtrace*"
           "*grep*" "*Compile-Log*" "*Shell Command Output*" "*compilation*"
           "*Occur*" "*log*" "*epic output*" "*git-status*"
           "*Async Shell Command*" "*save*" "*piped*" "*Async Shell Command Output*"
           "*cs*" "*Ack-and-a-half*" "*coffee-compiled*"
           )))
    (mapcar 'kill-buffer-if-exists buffers)))

(defun dthurn-code-assist ()
  "Mode-appropriate code assistance, stuff that's too slow to invoke through
  auto-complete"
  (interactive)
  (cond
   ((eq major-mode 'python-mode) (rope-code-assist nil))
   (t (indent-for-tab-command))))

;; (defun dthurn-eshell-previous-matching-input ()
;;   "Completes to the previous matching input in *eshell*, allows multiple calls
;;   to move through previous matching inputs."
;;   (interactive)
;;   (if (eq last-command 'dthurn-previous-input)
;;     (incf dthurn-previous-matching-input-prefix)
;;     (setq dthurn-previous-matching-input-prefix 1))
;;   (dlog dthurn-previous-matching-input-prefix)
;;   (let ((current-prefix-arg dthurn-previous-matching-input-prefix))
;;     (call-interactively 'eshell-previous-matching-input-from-input)))

(defun dthurn-eshell-previous-matching-input-from-input (arg)
  "Search backwards through input history for match for current input.
\(Previous history elements are earlier commands.)
With prefix argument N, search for Nth previous match.
If N is negative, search forwards for the -Nth following match."
  (interactive "p")
  (if (not (memq last-command '(dthurn-previous-input 'dthurn-next-input)))
      ;; Starting a new search
      (setq eshell-matching-input-from-input-string
	    (buffer-substring (save-excursion (eshell-bol) (point))
			      (point))
	    eshell-history-index nil))
  (eshell-previous-matching-input
   (concat "^" (regexp-quote eshell-matching-input-from-input-string))
   arg))

(defun dthurn-previous-input ()
  "Completes to previous matching input"
  (interactive)
  (cond ((eq major-mode 'shell-mode)
         (call-interactively 'comint-previous-matching-input-from-input))
        ((eq major-mode 'slime-repl-mode)
         (call-interactively 'slime-repl-backward-input))
        ((eq major-mode 'eshell-mode)
         (call-interactively 'dthurn-eshell-previous-matching-input-from-input))
        ((eq major-mode 'haskell-interactive-mode)
         (haskell-interactive-mode-history-toggle 1))
        ((eq major-mode 'cider-repl-mode)
          (cider-repl-previous-input))
        ((eq major-mode 'term-mode)
         (call-interactively 'term-previous-input))))

(defun dthurn-next-input ()
  "Shows the next input from history"
  (interactive)
  (cond ((eq major-mode 'shell-mode)
         (call-interactively 'comint-next-input))
        ((eq major-mode 'slime-repl-mode)
         (call-interactively 'slime-repl-forward-input))
        ((eq major-mode 'eshell-mode)
         (call-interactively 'eshell-next-input))
        ((eq major-mode 'cider-repl-mode)
          (cider-repl-next-input))
        ((eq major-mode 'haskell-interactive-mode)
         (haskell-interactive-mode-history-toggle -1))))

(defun dthurn-eval-buffer ()
  "Evaluate current emacs elisp buffer"
  (interactive)
  (save-buffer)
  (eval-buffer))

(defun dthurn-save-buffer ()
  "Saves the buffer"
  (interactive)
  (cond
   (t (save-buffer))))

(defun dthurn-tab (&rest args)
  (interactive)
  (cond
    ((minibufferp)
      (unless (minibuffer-complete)
        (dabbrev-expand nil)))
    ((member major-mode '(eshell-mode))
      (pcomplete))
    ((member major-mode '(shell-mode))
      (completion-at-point))
    (mark-active
      (let ((deactivate-mark nil))
        (indent-rigidly (region-beginning) (region-end) 2)))
    ((member (char-before) '(nil ?\ ?\n ?\t))
      (let ((old (point)))
        (beginning-of-line)
        (insert-tab)
        (goto-char (+ old 2))))
    (t
      (company-complete-common))))

(defun dthurn-backward-tab (&rest args)
  (interactive)
  (cond
    (mark-active
      (let ((deactivate-mark nil))
        (indent-rigidly (region-beginning) (region-end) -2)))
    (t
      (let ((old (point)))
        (beginning-of-line)
        (delete-char 2)
        (goto-char (- old 2))))))

(defvar skip-windows '("*cljsbuild*"))

(defun dthurn-other-window ()
  (interactive)
  (other-window 1)
  (while (member (buffer-name) skip-windows)
    (other-window 1)))

(defun dthurn-previous-window ()
  (interactive)
  (other-window -1)
  (while (member (buffer-name) skip-windows)
    (other-window -1)))

(defvar sober-mode-map (make-keymap)
  "Keymap for sober-mode.")

(defmacro sober-map-key (key command)
  `(progn
     (global-set-key (kbd ,key) ,command)
     (define-key sober-mode-map (kbd ,key) ,command)))

(sober-map-key "C-SPC" 'ace-jump-mode)
(sober-map-key "TAB" 'dthurn-tab)
(sober-map-key "<backtab>" 'dthurn-backward-tab)

;; Top Row
(sober-map-key "C-q" 'move-to-window-line)
(sober-map-key "C-w" 'dthurn-page-up)
(sober-map-key "C-e" 'end-of-line)
(sober-map-key "C-r" 'backward-kill-word)
(sober-map-key "C-t" 'dthurn-other-window)
(sober-map-key "C-S-t" 'other-window)
(sober-map-key "C-y" 'goto-line)
(sober-map-key "C-u" 'yank)
(sober-map-key "M-t" 'dthurn-page-down) ; REMAPPED AT OS LEVEL TO SEND C-i
(sober-map-key "C-o" 'dthurn-open)
(sober-map-key "C-p" 'dthurn-previous-window)
(sober-map-key "C-S-p" (lambda () (interactive) (other-window -1)))

;; Middle Row
(sober-map-key "C-a" 'dthurn-bol)
(sober-map-key "C-d" 'forward-char)
(sober-map-key "C-f" 'forward-word)
(sober-map-key "C-g" 'keyboard-escape-quit)
(sober-map-key "C-j" 'next-line)
(sober-map-key "<down>" 'next-line)
(sober-map-key "C-k" 'previous-line)
(sober-map-key "<up>" 'previous-line)
(sober-map-key "C-l" 'backward-word)
(sober-map-key "C-;" 'backward-char)

;; Bottom Row
(sober-map-key "C-z" 'dthurn-next-input)
(sober-map-key "C-v" 'dthurn-save-buffer)
(sober-map-key "C-b" 'dthurn-previous-input)
(sober-map-key "C-n" 'kill-line)
(sober-map-key "M-y" 'delete-char) ; REMAPPED AT OS LEVEL TO SEND C-m
(sober-map-key "C-," 'smex)
(sober-map-key "M-[ a" 'smex) ; REMAPPED AT OS LEVEL TO SEND C-, in TERMINAL
(sober-map-key "C-/" 'undo)

;; Top Row (Meta)
(sober-map-key "M-q" 'save-buffers-kill-terminal)
(sober-map-key "s-q" 'save-buffers-kill-terminal)
(sober-map-key "M-w" 'dthurn-kill-starred-buffers)
(sober-map-key "s-w" 'dthurn-kill-starred-buffers)
(sober-map-key "M-e" 'indent-sexp) ; Override as a formatting command
(sober-map-key "s-e" 'indent-sexp) ; Override as a formatting command
(sober-map-key "M-r" 'forward-paragraph)
(sober-map-key "s-r" 'forward-paragraph)
(sober-map-key "M-u" 'beginning-of-buffer)
(sober-map-key "s-u" 'beginning-of-buffer)
(sober-map-key "M-i" 'dthurn-eval-buffer) ; Override as a compile command
(sober-map-key "s-i" 'dthurn-eval-buffer) ; Override as a compile command
(sober-map-key "M-p" 'dthurn-goto-symbol)
(sober-map-key "s-p" 'dthurn-goto-symbol)

;; Middle Row
(sober-map-key "M-a" 'mark-whole-buffer)
(sober-map-key "s-a" 'mark-whole-buffer)
(sober-map-key "M-s" 'delete-other-windows)
(sober-map-key "s-s" 'delete-other-windows)
(sober-map-key "M-d" 'kill-word)
(sober-map-key "s-d" 'kill-word)
(sober-map-key "M-f" 'forward-sexp)
(sober-map-key "s-f" 'forward-sexp)
(sober-map-key "M-g" 'end-of-buffer)
(sober-map-key "s-g" 'end-of-buffer)
(sober-map-key "M-h" 'backward-sexp)
(sober-map-key "s-h" 'backward-sexp)
(sober-map-key "M-j" 'ido-switch-buffer)
(sober-map-key "s-j" 'ido-switch-buffer)
(sober-map-key "M-k" 'set-mark-command)
(sober-map-key "s-k" 'set-mark-command)
(sober-map-key "M-l" 'isearch-backward)
(sober-map-key "s-l" 'isearch-backward)
(sober-map-key "M-;" 'eval-expression)
(sober-map-key "s-;" 'eval-expression)

;; Bottom Row
(sober-map-key "M-z" 'undo)
(sober-map-key "M-x" 'kill-region)
(sober-map-key "s-z" 'undo)
(sober-map-key "s-x" 'kill-region)
(sober-map-key "M-c" 'copy-region-as-kill)
(sober-map-key "s-c" 'copy-region-as-kill)
(sober-map-key "M-v" 'clipboard-yank)
(sober-map-key "s-v" 'clipboard-yank)
(sober-map-key "M-n" 'backward-paragraph)
(sober-map-key "s-n" 'backward-paragraph)
(sober-map-key "M-." 'find-tag-other-window)
(sober-map-key "s-." 'find-tag-other-window)
(sober-map-key "M-/" 'dthurn-comment-or-uncomment-region-or-line)
(sober-map-key "s-/" 'dthurn-comment-or-uncomment-region-or-line)
(sober-map-key "M-`" 'other-window)
(sober-map-key "s-`" 'other-window)

(sober-map-key "C-x C-f" 'find-file-at-point)
(sober-map-key "C-\\" 'universal-argument)
(sober-map-key "C-c <down>" 'move-to-window-line)
(sober-map-key "C-c C-j" 'move-to-window-line)
(sober-map-key "C-c C-i" 'fix-init)
(sober-map-key "C-c C-h" 'java-println)
(sober-map-key "C-c C-p" 'balance-windows)
(sober-map-key "<C-tab>" 'dthurn-code-assist)
(sober-map-key "C-c M-y" 'call-last-kbd-macro)

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
