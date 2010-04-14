;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration for jdee - Java Development Environment for Emacs             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Specify the JDK location for OSX
(when (eq system-type "darwin")
  (setq jde-jdk-registry (quote (("1.6" . "/System/Library/Frameworks/JavaVM.framework")))))

(setq jde-jdk '("1.6"))

(require 'cedet)
(require 'jde)
