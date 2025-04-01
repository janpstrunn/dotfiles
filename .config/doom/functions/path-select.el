;; "Insert a file path with completion.
;; If the cursor is on a file path, replace it with the selected path.
;; Otherwise, start completion from the current directory."

(defun my-insert-file-path ()
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'filename))
         (default-path (if bounds
                           (buffer-substring-no-properties (car bounds) (cdr bounds))
                         default-directory))
         (new-path (read-file-name "Insert file: " default-path)))
    (when (string-prefix-p "~" default-path)
      (setq new-path (concat "~" (string-remove-prefix (expand-file-name "~") new-path))))
    (when bounds
      (delete-region (car bounds) (cdr bounds)))
    (insert new-path)))

(global-set-key (kbd "C-x C-a") 'my-insert-file-path)
