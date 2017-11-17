(use-package org :ensure t
  :config
  (setq org-goto-interface 'outline-path-completion)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-return-follows-link t)

  (defun org-id-complete-link (&optional arg)
    "Create an id: link using completion"
    (concat "id:"
           (org-id-get-with-outline-path-completion)))

  (org-link-set-parameters "id"
                           :complete 'org-id-complete-link))
