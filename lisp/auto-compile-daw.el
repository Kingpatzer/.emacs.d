;;
;; try to ensure everything is fast as posible
;;

(use-package auto-compile
  :init (setq load-prefer-newer t)
        (auto-compile-on-load-mode)
        (auto-compile-on-save-mode))
