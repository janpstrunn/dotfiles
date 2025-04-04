(defun my/org-insert-clipboard-image ()
  "Insert image in WEBP from clipboard"
  (interactive)
  (let* ((script-path "./clipboard-to-webp.sh")
         (fullpath (string-trim (shell-command-to-string script-path)))
         (relpath (file-relative-name fullpath (file-name-directory (buffer-file-name)))))
    (if (file-exists-p fullpath)
        (insert (format "%s\n" relpath))
      (message "Error: Image file was not created."))))
