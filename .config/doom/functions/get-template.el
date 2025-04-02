;; Author: https://github.com/librephoenix

(defun org-new-file-from-template ()
  "Copy a template from ~/templates into a timestamped unique-named file in the
same directory as the current org buffer and insert a link to this file."
  (interactive)
  (let* ((template-file (completing-read "Template file: " (directory-files "~/templates" nil "^[^.].*")))
         (template-path (expand-file-name template-file "~/templates"))
         (target-dir (expand-file-name "files/" (file-name-directory (buffer-file-name))))
         (filename (concat target-dir
                           (file-name-nondirectory (buffer-file-name))
                           "_"
                           (format-time-string "%Y%m%d_%H%M%S")
                           (file-name-extension template-file t))))
    (unless (file-exists-p target-dir)
      (make-directory target-dir t))
    (copy-file template-path filename)
    (let ((prettyname (read-from-minibuffer "Pretty name: ")))
      (insert (format "[[./files/%s][%s]]" (file-name-nondirectory filename) prettyname)))
    (org-display-inline-images)))

(map! :leader
      :desc "Create a new file from a template and insert a link at point"
      "i t" #'org-new-file-from-template)
