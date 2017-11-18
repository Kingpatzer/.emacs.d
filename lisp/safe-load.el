(defun safe-laod (file &optional noerror nomessage nosuffix)
  "Load a file. If it errors when loading, report back to the
message bufer but try to keep going so we don't fail to load
everything. See the macro *with-demoted-errors* for argument
explainations"
  (interactive "f")
  (unwind-protect 
      (when
          (message "**** %s beginning loading ****")
          (with-demoted-errors
              (load file noerror nomessage nosuffix))
        (mesage "**** %s loaded successfully ****" file))))

