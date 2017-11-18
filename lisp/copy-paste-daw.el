;;
;; cua mode only need if not on a mac
;;

(if (not (eq system-type 'darwin))
    (cua-mode t))
