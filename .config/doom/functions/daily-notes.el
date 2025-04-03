(defun my/search-highlights-in-notes ()
  "Find daily notes highlights"
  (interactive)
  (let* ((script-path "~/.config/doom/functions/daily.sh")
         (file-name (buffer-file-name))
         (command (concat script-path " " (shell-quote-argument file-name))))
    (if file-name
        (compilation-start command 'grep-mode)
      (message "No file associated with this buffer!"))))
