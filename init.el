;;; package --- Summary:
;;;     This isn't a package, obviously, but I like to shut up
;;;     Flycheck.
;;;
;;; Commentary:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;      This is my dotemacs file
;;
;;      Most of what is here has been stolen from someone else
;;      feel free to do the same to me
;;
;;      if you add any fun hacks, let me know about them.  I'm
;;      always looking for new and better ways to do things
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

;;
;; Keep Emacs from mucking with my config.
;;

					; (package-initialize)

;;
;; try to avoid custom-file crap
;;

(setq custom-file
      (concat user-emacs-directory "custom.el"))

;;
;; who am i
;;

(setq user-full-name "David Wagle"
      user-mail-address "david.wagle@gmail.com")

;;
;; speed up load time
;;

(defun reset-gc-cons-threshold ()
  "Return the garbage collection threshold to default values."
  (setq gc-cons-threshold
	(car (get 'gc-cons-threshold 'standard-value))))

(setq gc-cons-threshold (* 64 1024 1024))

(add-hook 'after-init-hook 'reset-gc-cons-threshold)

;;; turn on some debugging

(setq debug-on-error t)


;;
;; System niceties
;;

;; don't accidentally sleep Emacs

(global-set-key (kbd "C-z") nil)

;; No window decorations

(tool-bar-mode 0)
(menu-bar-mode 0)
(set-scroll-bar-mode nil)
(setq inhibit-startup-screen t)
(fringe-mode '(8 . 8))

;; Nice fairly universal font

(set-frame-font "DejaVu Sans Mono-15")
(add-to-list 'initial-frame-alist
	     '(font . "DejaVu Sans Mono-15"))
(add-to-list 'default-frame-alist
	     '(font . "DejaVu Sans Mono-15"))

;; utf-8 everywhere

(prefer-coding-system 'utf-8)
;;(set-default-coding-systems 'utf-8)

(when (display-graphic-p)
  (setq x-select-request-type
	'(UTF-STRING COMPOUND_TEXT TEXT STRING)))

;; y or n is sufficient

(defalias 'yes-or-no-p 'y-or-n-p)

;;
;; make isearch wrap around
;;

(defadvice isearch-repeat (after isearch-no-fail activate)
  "Allow isearch to wrap if nothing found searching forawrd.
Deactivates at first failt o prevent an infinite loop."
  (unless isearch-success
    (ad-disable-advice 'search-repate 'after 'isearch-no-fail)
    (ad-activate 'isearch-repeat)
    (isearch-repeat (if isearch-foward 'forward))
    (ad-enable-advice 'isearch-repeat 'after 'search-no-fail)
    (ad-activate 'isearch-repeat)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 					        ;;
;;  linum in programing modes and add padding   ;;
;;  					        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'prog-mode-hook 'linum-mode)
(defvar linum-format)
(setq linum-format "%4d \u2502")

;;
;; highlight current line in programming modes
;;

(add-hook 'prog-mode-hook 'hl-line-mode)


;;
;; minibuffer stuff
;;

(require 'uniquify)


;; give buffers better unique names


(setq
 uniquify-buffer-name-style 'forward    ; names use / for delimiter
 uniquify-after-kill-buffer-p t         ; rationalize after kill
 uniquify-ignore-buffers-re "^\\*")     ; ignore system buffers

;; the minibuffer needs some tweaks

(setq enable-recursive-minibuffers nil)  ;  allow mb cmds in the mb
(setq max-mini-window-height .25)        ;  max 2 lines
(setq minibuffer-scroll-window nil)      ; no scrolling in mb
(setq resize-mini-windows nil)           ; no resizing the mb


;;
;; set environment paths
;;

(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setq exec-path (append '("/usr/local/bin") exec-path))

;;
;; recent file
;;

(defvar recentf-max-menu-items) 	;dummy variable to kill warnings
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;;
;; Make sure we have use-package
;;

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/elpa/use-package/")
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  My functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;      Load Packages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package ace-flyspell
  :ensure t
  :config
  (ace-flyspell-setup))

(use-package ace-jump-mode
  :ensure t
  :bind (("C-'" . ace-jump-mode)))

(use-package ace-jump-helm-line
  :ensure t
  :bind ((:map helm-map
	       ("C-'" . ace-jump-helm-line))))

(use-package ace-window
  :ensure t
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (
	 ("M-'" . ace-window)))

(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

(use-package all-the-icons
  :ensure t)

(use-package anzu
  :ensure t
  :config
  (global-anzu-mode t))

(use-package auctex
  :ensure t
  :defer t
  :defines (TeX-auto-save
	    TeX-parse-self
	    reftex-plug-into-AUCTex)
  :config
  (setq TeX-auto-save t
	TeX-parse-self t
	reftex-plug-into-AUCTex t))

(use-package auto-compile
  :ensure t
  :init (setq load-prefer-newer t)
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package auto-package-update
  :ensure t
  :init (auto-package-update-maybe))

(use-package bookmark+
  :ensure t)


(use-package cider
  :ensure t
  :defer t
  :defines (cider-repl-pop-to-buffer-on-connect
	    cider-repl-use-pretty-printing)
  :config (setq cider-repl-pop-to-buffer-on-connect 'display-only
		cider-repl-use-pretty-printing t)
  (add-hook 'cider-mode-hook 'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-repl-mode-hook 'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-repl-mode-hook 'paredit-mode))

(use-package circe
  :ensure t
  :defer t
  :config
  (setq circe-network-options '(("Freenode"
				 :tls t
				 :nick "kingpatzer"
				 :sasl-username "kingpatzer"
				 :sasl-password "shadowfax"
				 :channels ("#emacs" "#emacs-circe"
					    "#nethack")))))

(use-package clojure-cheatsheet
  :ensure t
  :defer t)

(use-package clojure-mode
  :ensure t
  :defer t
  :config
  (add-hook 'clojure-mode-hook 'paredit-mode))

;; (use-package clojure-snippets
;;   :ensure t
;;   :defer t)

;; (use-package common-lisp-snippets
;;   :ensure t
;;   :defer t)

(use-package company
  :ensure t
  :defines (company-idle-delay
	    company-tooltip-limit
	    company-minimum-prefix-length
	    company-tooltip-flip-when-above)
  :config
  (setq company-idle-delay 0.5
	company-tooltip-limit 10
	company-minimum-prefix-length 2
	company-tooltip-flip-when-above t)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-auctex
  :ensure t)

(use-package company-bibtex
  :ensure t)

(use-package company-jedi
  :ensure t)

(use-package company-math
  :ensure t)

(use-package company-shell
  :ensure t)

(use-package company-try-hard
  :ensure t
  :bind ("C-z" . company-try-hard))

(use-package cycle-themes
  :ensure t
  :defer t
  :defines cycle-themes-theme-list
  :init (setq cycle-themes-theme-list
	      '(leuven
		zenburn
		professional
		leuven-dark
		solarized-light
		tango-dark
		tango
		solarized-dark))
  (add-hook 'cycle-themes-after-cycle-hook
	    #'(lambda ()
		(dolist
		    (frame
		     (frame-list)
		     (set-face-attribute 'fringe frame
					 :background (face-background 'default))))))
  :config (cycle-themes-mode)
  :bind (("C-x T T" . cycle-themes)))

(use-package diminish
  :ensure t
  :config (require 'diminish))

(use-package dired+
  :ensure t
  :defer t
  :config (require 'dired+))

(use-package ebib
  :ensure t
  :defer t
  :init
  :bind (("C-c e" . ebib)))

(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :ensure t
    :init (exec-path-from-shell-initialize)))

(use-package eyebrowse
  :ensure t
  :config (require 'eyebrowse))


(use-package flycheck
  :ensure t
  :defer t
  :init (global-flycheck-mode))

(use-package flyspell
  :ensure t
  :defer t
  :init (dolist (hook '(text-mode-hook))
	  (add-hook hook (lambda () (flyspell-mode 1))))
  :bind (("C-;" . flyspell-mode)
	 ("C-:" . flyspell-check-next-highlighted-word)))

(use-package golden-ratio
  :ensure t)


(use-package helm
  :ensure t
  :defer t
  :defines (helm-net-prefer-curl
	    helm-split-window-inside-p
	    helm-ff-search-library-in-sexp
	    helm-scroll-amount
	    helm-ff-file-name-history-use-recent
	    helm-echo-input-in-header-line
	    helm-command-map
	    helm-apropos-fuzzy-match
	    helm-buffers-fuzzy-match
	    helm-eshell-fuzzy-match
	    helm-M-x-fuzzy-match
	    helm-recentf-fuzzy-match
	    helm-mini-default-sources)
  :init
  (require 'helm-config)
  (require 'helm-eshell)
  (require 'helm-bookmark)
  (helm-mode t)
  (setq helm-net-prefer-curl t
	helm-split-window-inside-p t
	helm-ff-search-library-in-sexp t
	helm-scroll-amount 8
	helm-ff-file-name-history-use-recent t
	helm-echo-input-in-header-line  t
	helm-follow-mode-persistent t
	helm-display-header-line nil
	helm-apropos-fuzzy-match t
	helm-buffers-fuzzy-match t
	helm-eshell-fuzzy-match t
	helm-M-x-fuzzy-match t
	helm-recentf-fuzzy-match t
	helm-mini-default-sources '(helm-source-buffers-list
				    helm-source-recentf
				    helm-source-bookmarks
				    helm-source-buffer-not-found))
  :bind
  (("M-x"        . helm-M-x)
   ("C-x C-m"    . helm-M-x)
   ("M-y"        . helm-show-kill-ring)
   ("C-x b"      . helm-mini)
   ("C-x C-b"    . helm-buffer-list)
   ("C-x C-f"    . helm-find-files)
   ("C-h f"      . helm-apropos)
   ("C-h r"      . helm-info-emacs)
   ("C-h C-l"    . helm-locate-library)
   ("C-c f"      . helm-recentf)
   :map minibuffer-local-map
   ("C-c C-l"    . helm-minibuffer-history)
   ("ESC"        . minibuffer-keyboard-quit)
   :map isearch-mode-map
   ("C-o"        . helm-occur-from-isearch)
   :map helm-command-map
   ("o"          . helm-occur)
   ("g"          . helm-do-grep)
   ("C-c w"      . helm-wikipedia-suggest)
   ("SPC"        . helm-all-mark-rings)
   ("C-c h"      . helm-execute-persistent-action)
   ("h"          . helm-begging-of-buffer)
   ("j"          . helm-next-line)
   ("k"          . helm-previous-line)
   ("j"          . helm-end-of-buffer)
   ("g"          . helm-begging-of-buffer)
   ("G"          . helm-end-of-buffer)
   ("K"          . helm-scroll-other-window-down)
   ("J"          . helm-scroll-other-window-up)
   ("m"          . helm-toggle-visible-mark)
   ("t"          . helm-toggle-all-marks)
   ("U"          . helm-unmark-all)
   ("i"          . nil)
   ("}"          . helm-next-source)
   ("{"          . helm-previous-source)
   ("H"          . helm-help)
   ("v"          . helm-execute-persistent-action)
   ("d"          . helm-persistent-delete-marked)
   ("f"          . helm-follow-mode)
   ("ESC"        . helm-keyboard-quit)))


(use-package helm-descbinds
  :ensure t
  :init (helm-descbinds-mode t))

(use-package helm-describe-modes
  :ensure t
  :bind ("C-h m" . helm-describe-modes))

(use-package helm-projectile
  :ensure t
  :bind ("C-c p h" . helm-projectile))

(use-package ispell
  :ensure t
  :defer t
  :init (setq ispell-dictionary "en_US"
	      ispell-program-name "aspell"))

(use-package info+
  :ensure t
  :defer t)

(use-package key-chord
  :ensure t
  :defer t)


(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch-popup)))



(use-package org-bullets
  :ensure t)


(use-package org
  :ensure t
  :defer t
  :defines (org-tag-persistent-alist
	    org-agenda-ndays
	    org-agenda-show-all-dates
	    org-agenda-skip-scheduled-if-done
	    org-agenda-skip-deadlines-if-done
	    org-agenda-start-on-weekday
	    org-deadline-warning-days
	    org-agenda-with-colors
	    org-agenda-compact-blocks
	    org-agenda-remove-tags)
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-directory (expand-file-name "~/Nextcloud/Documents/Org")
	org-agenda-files '("~/Nextcloud/Documents/Org/Home.org"
			   "~/Nextcloud/Documents/Org/Work.org"
			   "~/Nextcloud/Documents/Org/index.org"
			   "~/Nextcloud/Documents/Org/Other.org"
			   "~/Nextcloud/Documents/Org/School.org"
			   "~/Nextcloud/Documents/Org/Personal.org")
	org-todo-keywords '((sequence "IDEA(i)" "TODO(t)"
				      "STARTED(s)" "NEXT(n)"
				      "WAITING(w)" "|" "DONE(d)")
			    (sequence "|" "CANCELLED(C)"
				      "DELEGATED(l)" "SOMEDAY(m)"))
	org-tag-persistent-alist '((:startgroup . nil)
				   ("HOME" . ?h)
				   ("RESEARCH" . ?r)
				   ("WRITING" . ?w)
				   ("READING" . ?d)
				   (:endgroup . nil)
				   (:startgroup . nil)
				   ("LISP"    . ?p)
				   ("CLOJURE" . ?c)
				   ("PYTHON"  . ?n)
				   ("R"       . ?r)
				   (:endgroup . nil)
				   (:startgroup . nil)
				   ("URGENT"   . ?u)
				   (:endgroup  . nil))
	
	org-agenda-ndays 14
	org-agenda-show-all-dates t
	org-agenda-skip-deadlines-if-done t
	org-agenda-skip-scheduled-if-done t
	org-agenda-start-on-weekday nil
	org-deadline-warning-days 3
	org-agenda-with-colors t
	org-agenda-compact-blocks t
	org-agenda-remove-tags nil)
  (add-to-list 'ispell-skip-region-alist '((":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:")
					   ("#\\+BEGIN_SRC" . "#\\+END_SRC")
					   '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE")))
  :bind (("C-x o"     . nil)
	 ("C-x o l"   . org-store-link)
	 ("C-x o a"   . org-agenda)
	 ("C-x o c"   . org-capture)))


(use-package paredit
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode))

(defvar persp-auto-kill-buffer-on-remove)

(use-package persp-mode
  :ensure t
  :after workgroup
  :init (with-eval-after-load "persp-mode-autoloads"
	  (setq persp-auto-kill-buffer-on-remove 'kill-weak)
	  (add-hook 'after-init-hook #'(lambda () persp-mode 1))))

(use-package projectile
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package request
  :ensure t)

(use-package shell-pop
  :ensure t
  :defines (shell-pop-check-internal-mode
	    shell-pop-set-window-position
	    shell-pop-set-window-height
	    shell-pop-internal-mode-shell)
  :config (setq shell-pop-internal-mode "ansi-term"
		shell-pop-internal-mode-shell "/bin/zsh"
		shell-pop-set-window-height 40
		shell-pop-set-window-position "bottom")
  :bind ([f8] . shell-pop))


(use-package slime
  :ensure t
  :defines (inferior-lisp-program
	    slime-contribs)
  :init (setq inferior-lisp-program "/usr/local/bin/sbcl"
	      slime-contribs '(slime-fancy
			       slime-tramp
			       slime-asdf))
  :bind (:map slime-prefix-map
	      ("M-h" . slime-documentaion-lookup))
  :config
  (slime-require :swank-listener-hooks))

(use-package slime-company
  :ensure t)

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config))

(use-package spaceline-all-the-icons
  :after spaceline
  :ensure t
  :config (spaceline-all-the-icons-theme)
  (spaceline-toggle-all-the-icons-bookmark-on)
  (spaceline-toggle-all-the-icons-dedicated-on)
  (spaceline-toggle-all-the-icons-buffer-position-on)
  (spaceline-toggle-all-the-icons-narrowed-on)
  (spaceline-all-the-icons--setup-anzu)
  (spaceline-all-the-icons--setup-package-updates)
  (spaceline-all-the-icons--setup-git-ahead)
  (spaceline-all-the-icons--setup-paradox)
  (spaceline-all-the-icons--setup-neotree))

(use-package winum
  :ensure t
  :defines (winum-auto-setup-mode-line)
  :config (setq winum-auto-setup-mode-line nil)
  (winum-mode))

(use-package workgroups
  :ensure t
  :defines wq-morph-on
  :config (require 'workgroups)
  (workgroups-mode)
  (setq wq-morph-on nil)
  :bind ("C-c w" . wg-prefix-key))

(use-package yahoo-weather
  :ensure t
  :config (setq yahoo-weather-use-F t
		yahoo-weather-update-interval 360)
  (require 'request)
  (defun daw-set-yahoo-weather-location ()
    (request "https://ipinfo.io"
             :parser 'json-read
             :success (cl-function
		       (lambda (&key data &allow-other-keys)
                         (setq yahoo-weather-location (format "%s, %s"
							      (assoc-default 'city data)
							      (assoc-default 'region data)))))))
  (run-with-timer 0 (* 30 60) 'daw-set-yahoo-weather-location)
  (yahoo-weather-mode))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;      Load Themes
;;
;;      These must match "cycle-theme" use-package definition
;;      at a minimum.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;;load safe theme customize data
;;

(load custom-file)

;;
;; load themes
;;

(use-package leuven-theme
  :ensure t
  :config (progn (load-theme 'leuven t t)
		 (load-theme 'leuven-dark t t)))

(use-package professional-theme
  :ensure t)

(use-package solarized-theme
  :ensure t
  :config (progn (load-theme 'solarized-dark t t)
		 (load-theme 'solarized-light t t)))

(use-package zenburn-theme
  :ensure t
  :config (progn (load-theme 'zenburn t t)))


(use-package color-theme-tango
  :ensure t
  :config (progn (load-theme 'tango t t )
		 (load-theme 'tango-dark t t)))
;;
;; turn debugging off
;;

(setq debug-on-error nil)
(provide 'init)
;;; init.el ends here
