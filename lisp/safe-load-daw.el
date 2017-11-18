(defun safe-load (file &optional noerror nomessage nosuffix mustsuffix)
  "Load a file. If it errors when loading, report back to the
message bufer but try to keep going so we don't fail to load
everything. See the macro *with-demoted-errors* for argument
explainations"
  (interactive "f")
  (unwind-protect 
      (when
          (message "**** %s beginning loading ****" file)
          (with-demoted-errors
              (load file noerror nomessage nosuffix mustsuffix))
        (message "**** %s loaded successfully ****" file))))

