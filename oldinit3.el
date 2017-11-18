
;;; Commentary:
;;; init.el -- my configuration entry point
;;
;; Copyright (c) 2017 David Wagle
;;
;; Author: David Wagle URL: http://davidwagle.com Version 0.0.1
;; Keywords: Emacs, startup, init
;;
;;
;;
;;This file is to set up the default load paths and other necessary
;;files to start Emacs in a sane fashioon
;;
;; It is distributed under the terms of the GNU Genearl Public
;; License version 3.
;;
;; This program is distributed without any warranted, including
;; any implied warranty of merchantability or fitness for a
;; particular purpose.
;;
;;
;;
;;; Code:

;; The follownig line must exist and must be commented out
;; or Emacs will do bad things to this file.  GNU calls this
;; a feature.  We'll leave the judgement of that claim to
;; the audience.
;; 
					;(package-initialize)
;; MODIFY THIS IF YOU WANT TO USE A DIFFERENT ORG FILE NAME
;; note that this does not include the file extension
;; that will be added by the system depending on if



(defvar daw-starting-org-filename "settings.org"
  "This is the name of the file that will be loaded to compolete initialization.")


;; set up directory paths
(defvar daw-dir (file-name-directory load-file-name)
  "This is the base directory of the init.el file.")



(defvar daw-starting-org-file
  (expand-file-name
   (concat daw-dir
	   (file-name-nondirectory daw-starting-org-filename)))
  "This is the fully qualified path name of the .ORG file that in which the next part of initialization is kept.")

(defvar daw-starting-org-file-base
  (file-name-base daw-starting-org-filename)
  "This is the filename sans any extensions.")

(defvar daw-starting-org-file-tangled
  (expand-file-name
   (concat daw-starting-org-file-base ".el")
   daw-dir)
  "This is the fuly-qualified  name of the '.el' file that is produced by org-babel-tangle-file being run on the .ORG configuration file.")

(defvar daw-starting-org-file-compiled
  (expand-file-name
   (concat daw-starting-org-file-base ".elc")
   daw-dir)
  "This is the fuly qualified name of the '.elc' file that is produced by byte-compiling the '.el' file produced from the .ORG config file.")

(message "Checking to see if a %s file exists" daw-starting-org-file-compiled)

(if (file-exists-p daw-starting-org-file-compiled)
    (progn
      (message "Loading %s" daw-starting-org-file-compiled)
      (load daw-starting-org-file-compiled))
  (progn
    (let ((src daw-starting-org-file)
	  (dst daw-starting-org-file-tangled))
      (message "Running bable-tangle on %s" daw-starting-org-file)
      (when (file-newer-than-file-p src dst)
	(call-process
	 (concat invocation-directory invocation-name)
	 nil nil t
	 "-q" "--batch" "--eval" "(require 'ob-tangle)"
	 "--eval" (format "(org-babel-tangle-file \"%s\" \"%s\")" src dst)))
      (require 'bytecomp)
      (byte-recompile-file dst nil 0 t)
      (load daw-starting-org-file-compiled))))






(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (powerline flyspell-correct-helm flycheck-pos-tip flyspell-correct-popup flycheck-popup-tip paredit rainbow-delimiters councel flycheck-clojure flycheck counsel swiper ivy theme-package solarized-theme professional-theme zenburn-theme nil use-package dash)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-bullet-face ((t (:foreground "burlywood" :weight normal :height 1.5)))))
