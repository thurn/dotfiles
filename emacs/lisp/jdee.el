;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration for jdee - Java Development Environment for Emacs             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq jde-jdk-registry (quote (("1.6" . "/System/Library/Frameworks/JavaVM.framework/Home"))))
(setq jde-jdk '("1.6"))

(require 'cedet)
(require 'jde)
