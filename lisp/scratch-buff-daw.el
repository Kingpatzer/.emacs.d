  (defun daw-fortune-scratch-message ()
  "insert fortune into buffer if fortune exists on system"
    (interactive)
    (let ((fortune
           (when (executable-find "fortune")
             (with-temp-buffer
               (shell-command "fortune" t)
               (let ((comment-start ";;"))
                 (comment-region (point-min) (point-max)))
               (delete-trailing-whitespace (point-min) (point-max))
               (concat (buffer-string) "\n")))))
      (if (called-interactively-p 'any)
          (insert fortune)
        fortune)))

  (let ((fortune (daw-fortune-scratch-message)))
    (when fortune
      (setq initial-scratch-message fortune)))