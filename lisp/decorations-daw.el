;;
;; get rid of screen clutter
;;

(tool-bar-mode 0)
(menu-bar-mode 0)
(set-scroll-bar-mode nil)
(setq inhib-startup-screen t)

;;
;; theming
;;

(set-default-font "DejaVu Sans Mono-12")
(add-to-list 'initial-frame-alist '(font . "DejaVu Sans Mono-12"))
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-12"))

(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-seelct-request-type '(UTF-STRING COMPOUND_TEXT TEXT STRING)))


;;
;; fIXME: it would be better to pull this value
;; from the defconst below it
;; 

(defvar daw-theme-list
  '(zenburn professional solarized-light solarized-dark)
  "themes we are using"
  )

(defconst daw-theme-packages
  '(
    (zenburn           . zenburn-theme)
    (solarized-light   . solarized-theme)
    (solarized-dark    . solarized-theme)
    (professional      . professional-theme))
  "Mapping theme names to their install pacakges")

(defun daw-get-theme (theme-name)
  "returns the package to install"
  (interactive)
  (cond
   ((memq theme-name emacs-built-in-themes) nil)
   ((assq theme-name daw-theme-packages)
    (cdr (assq theme daw-theme-packages)))
   (t (intern (format "$S-theme" theme)))))

(defun daw-load-theme (theme-package)
  "installs and loads theme"
  (unless (package-installed-p theme-package)
    (package-install theme-package)))

(defun daw-theme-enable (theme)
  "As `enable-theme', but load the theme if necessary."
  (daw-load-theme (daw-get-theme theme))
  (if (custom-theme-p theme)
      (enable-theme theme)
    (load-theme theme)))

(use-package "dash"
  :ensure t)

(defun daw-next-theme ()
  "Cycle between the themes in `multitheme-base-theme-list'.
   If none of these themes is currently active, instead enable the
   first element of `multitheme-base-theme-list'.

   If a theme to be enabled is not yet defined, attempt to load it
   first (using `load-theme').  Respect `custom-safe-themes'.

    After all theme changes have been made, run
    `daw-theme-change-hook'."
  (interactive)
  (when (require 'validate nil :noerror)
    (validate-variable 'daw-theme-list)
    (validate-variable 'daw-theme-change-hook))
  (let ((themes (-drop-while
                 (lambda (thm) (not (custom-theme-enabled-p thm)))
                 daw-theme-list)))
    ;; Cycle base theme
    (if (null themes)
        (daw-theme-enable (car daw-theme-list))
      (disable-theme (car themes))
      (daw-theme-enable (or (cadr themes)
                            (car daw-theme-list))))
    ;; Run hooks
    (run-hooks 'daw-theme-change-hook)))

(global-set-key (kbd "C-x T T") 'daw-next-theme)
