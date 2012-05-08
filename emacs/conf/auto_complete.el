;; Configuration for auto-complete-mode
(require 'auto-complete)
(require 'auto-complete-config)
;(add-to-list 'ac-dictionary-directories "~/emacs/dict")

(ac-config-default)

(setq ac-completing-map
      '(keymap
        (C-up . ac-quick-help-scroll-up)
        (C-down . ac-quick-help-scroll-down)
        (67108927 . ac-help)
        (M-f1 . ac-persist-help)
        (f1 . ac-help)
        (19 . ac-isearch)
        (27 keymap
            (57 . ac-complete-9)
            (56 . ac-complete-8)
            (55 . ac-complete-7)
            (54 . ac-complete-6)
            (53 . ac-complete-5)
            (52 . ac-complete-4)
            (51 . ac-complete-3)
            (50 . ac-complete-2)
            (49 . ac-complete-1)
            (16 . ac-quick-help-scroll-up)
            (14 . ac-quick-help-scroll-down)
            (67108927 . ac-persist-help)
            (112 . ac-previous)
            (110 . ac-next)
            (9 . auto-complete))
        (13 . ac-complete)
        (9 . ac-expand)))

;; (setq-default ac-sources
;;       '(ac-source-words-in-buffer))
;; (setq ac-sources
;;       '(ac-source-words-in-buffer))

 ;;;###autoload
;; (defun dthurn-turn-on-auto-complete-mode ()
;;   "Turns on auto-complete mode if the buffer is appropriate."
;;   (if (not (or (window-minibuffer-p)
;;                buffer-read-only
;;                (eq major-mode 'shell-mode)))
;;       (auto-complete-mode t)))

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

