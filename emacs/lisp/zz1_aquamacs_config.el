;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configurations for the Aquamacs user interface.                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In Aquamacs, consolidate frames
(when (fboundp 'tabbar-window-merge-windows)
  (tabbar-window-merge-windows))

;; In Aquamacs, disable tabs
(when (fboundp 'tabbar-mode)
  (tabbar-mode nil))
