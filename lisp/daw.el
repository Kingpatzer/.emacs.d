;;

(eval-when-compile (require 'cl-lib)) ; useful macros
(load "safe-load-daw" nil t)              ; error trapping

(safe-load "use-package-daw"      nil t ) ; load packages
(safe-load "auto-compile-daw"     nil t ) ; compile everything for speed
(safe-load "startup-daw"          nil t ) ; startup perf enhancements
                                          ; must load finalize-daw if used
(safe-load "env-daw"              nil t ) ; paths, cache
(safe-load "personal-daw"         nil t ) ; personal info
(safe-load "scratch-buff-daw"     nil t ) ; fortune in scratch buffer
(safe-load "decorations-daw"      nil t ) ; appearances
(safe-load "copy-paste-daw"       nil t ) ; C-x/c/v/z do what they should
(safe-load "buffers-daw"          nil t ) ; make buffers behave

(safe-load "finalize-daw"         nil t ) ; retrun to sane garbage collection
                                          ; and debug settings
