;; Configuration for predictive abbreviation mode

(require 'pabbrev)
(require 'popup)

(defun pabbrevx-suggestions-goto-buffer (suggestions)
  (let* ((candidates (mapcar 'car suggestions))
         (bounds (pabbrev-bounds-of-thing-at-point))
         (selection (popup-menu* candidates
                                 :point (car bounds)
                                 :scroll-bar t)))
    (when selection
      ;; modified version of pabbrev-suggestions-insert
      (let ((point))
        (save-excursion
          (progn
            (delete-region (car bounds) (cdr bounds))
            (insert selection)
            (setq point (point))))
        (if point
            (goto-char point))
        ;; need to nil this so pabbrev-expand-maybe-full won't try
        ;; pabbrev expansion if user hits another TAB after ac aborts
        (setq pabbrev-last-expansion-suggestions nil)
        ))))
      
(fset 'pabbrev-suggestions-goto-buffer 'pabbrevx-suggestions-goto-buffer)


