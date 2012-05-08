;; Configuration for ipython, to use ipython in python-mode

;(require 'python)
;; Configuration for PyMacs
;(require 'pymacs)

;(autoload 'pymacs-apply "pymacs")
;(autoload 'pymacs-call "pymacs")
;(autoload 'pymacs-eval "pymacs" nil t)
;(autoload 'pymacs-exec "pymacs" nil t)
;(autoload 'pymacs-load "pymacs" nil t)

;(pymacs-load "ropemacs" "rope-")
;(setq ropemacs-enable-autoimport t)

;(when (load "flymake" t)
  ;(defun flymake-pyflakes-init ()
    ;(let* ((temp-file (flymake-init-create-temp-buffer-copy
               ;'flymake-create-temp-inplace))
       ;(local-file (file-relative-name
            ;temp-file
            ;(file-name-directory buffer-file-name))))
      ;(list "pycheckers"  (list local-file))))
   ;(add-to-list 'flymake-allowed-file-name-masks
             ;'("\\.py\\'" flymake-pyflakes-init)))

;; Installation
;; Install Pymacs (Emacs part)
;; $ curl -L https://github.com/pinard/Pymacs/tarball/v0.24-beta2 | tar zx
;; $ cd pinard-Pymacs-016b0bc
;; $ make 
;; $ mkdir -p ~/.emacs.d/vendor/pymacs-0.24-beta2
;; $ cp pymacs.el ~/.emacs.d/vendor/pymacs-0.24-beta2/pymacs.el 
;; $ emacs -batch -eval '(byte-compile-file "~/.emacs.d/vendor/pymacs-0.24-beta2/pymacs.el")' 
;; Install Pymacs (Python part)
;; $ sudo pip install https://github.com/pinard/Pymacs/tarball/v0.24-beta2 
;; Install Ropemacs and Rope
;; $ sudo pip install http://bitbucket.org/agr/ropemacs/get/tip.tar.gz 
;; Edit ~/.emacs to use Ropemacs
;; (add-to-list 'load-path "~/.emacs.d/vendor/pymacs-0.24-beta2")
;; (require 'pymacs)
;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-autoimport t)
