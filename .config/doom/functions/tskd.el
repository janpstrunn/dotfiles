(defun my/tasks ()
  "Print all done tasks from date range by Taskwarrior"
  (interactive)
  (let* ((file-name (file-name-nondirectory (buffer-file-name)))
         (file-base (file-name-sans-extension file-name)))
    (shell-command (concat "~/scripts/__org-tskd.sh " file-base))))
