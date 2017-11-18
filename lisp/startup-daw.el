;; turn garbage collection down during init
;; to speed things up

(defun reset-gc-cons-threshold ()
  (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value))))

(setq gc-cons-threshold (* 64 1024 1024))

(add-hook 'after-init-hook 'reset-gc-cons-threshold)

(message "Garbage Collection reduced for startup")

;;; Temporarily disable the file name handler.

(setq default-file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil)

(defun reset-file-name-handler-alist ()
  (setq file-name-handler-alist default-file-name-handler-alist))

(add-hook 'after-init-hook 'reset-file-name-handler-alist)

;;; turn on some debugging

(setq debug-on-error t)
