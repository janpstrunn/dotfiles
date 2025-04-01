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

(when (require 'openwith nil 'noerror)
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4"
                  "avi" "wmv" "wav" "mov" "flv"
                  "ogm" "ogg" "mkv"))
               "mpv" '(file))
         (list (openwith-make-extension-regexp
                '("doc" "xls" "ppt" "odt" "ods" "odg" "odp"))
               "libreoffice" '(file))
         (list "\\.lyx" "lyx" '(file))
         (list "\\.chm" "kchmviewer" '(file))
         (list (openwith-make-extension-regexp '("kdenlive"))
               "kdenlive-accel" '(file))))
  (openwith-mode 1))
