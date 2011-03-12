;; Configuration for imenu

(require 'imenu)
(require 'cl)
(defun ido-goto-symbol ()
  "Will update the imenu index and then use ido to select a symbol to
navigate to."
  (interactive)
  (setq imenu-create-index-function 'imenu-default-create-index-function)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))
                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))
                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1
                                                                 'org-imenu-marker symbol))))
                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name
                                                                position))))))))
      (addsymbols imenu--index-alist)
      (if (not symbol-names)
          (ido-goto-symbol)
        (let* ((symbol-at-point (symbol-name (symbol-at-point)))
               (selected-symbol (ido-completing-read
                                 "Symbol? "
                                 (if (member symbol-at-point symbol-names)
                                     (cons symbol-at-point (remove-if
                                                            (lambda
                                                              (x) (string-equal x symbol-at-point))
                                                            symbol-names))
                                   symbol-names)))
               (position (cdr (assoc selected-symbol name-and-pos))))
          (if (markerp position)
              (goto-char position) (goto-char (overlay-start position))))))))