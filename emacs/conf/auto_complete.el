;; Configuration for auto-complete-mode
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/emacs/dict")

(setq ac-delay 0)

(setq-default ac-sources
      '(ac-source-words-in-buffer))
(setq ac-sources
      '(ac-source-words-in-buffer))

 ;;;###autoload
(defun dthurn-turn-on-auto-complete-mode ()
  "Turns on auto-complete mode if the buffer is appropriate."
  (if (not (or (window-minibuffer-p)
               buffer-read-only
               (eq major-mode 'shell-mode)))
      (auto-complete-mode t)))

;; (define-global-minor-mode dthurn-auto-complete-global-mode
;;   auto-complete-mode
;;   dthurn-turn-on-auto-complete-mode)
;; (dthurn-auto-complete-global-mode t)

;; (defvar ac-hphpd-cache nil)

;; (defun ac-hphpd-candidate ()
;;   (print "candidate")
;;   (princ ac-prefix)
;;   (list "foo_is_a_function("
;;         "bar_a_test("
;;         "baz_a_field"))

;; (ac-define-source hphpd
;;   '((init . (setq ac-hphpd-cache nil))
;;     (candidates . ac-hphpd-candidate)
;;     (prefix . "->\\(.*\\)")
;;     (requires . 0)
;;     (action . ac-start)
;;     (limit . nil)))

;; (defvar ac-test-source
;;   '((candidates . ac-hphpd-candidate)
;;     (prefix . "->\\(.*\\)")))

