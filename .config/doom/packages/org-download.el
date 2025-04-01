;; Author: https://github.com/librephoenix

(require 'org-download)

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

;; system-wm-type, wayland or x11? only should be considered if system-nix-profile is "personal" or "work"
(if (string= system-wm-type "wayland")
    (setq org-download-screenshot-method "grim -g \"$(slurp)\" %s")
  (setq org-download-screenshot-method "flameshot gui -p %s")
  )

(after! org-download
  (setq org-download-method 'directory))

(after! org
  (setq-default org-download-image-dir "img/"
                org-download-heading-lvl nil))

(add-to-list 'display-buffer-alist '("^*Async Shell Command*" . (display-buffer-no-window)))

(defun org-download-clipboard-basename ()
  (interactive)
  (setq org-download-path-last-dir org-download-image-dir)
  (setq org-download-image-dir (completing-read "directory: " (-filter #'f-directory-p (directory-files-recursively "." "" t)) nil t))
  (org-download-clipboard (completing-read "basename: " '() nil nil))
  (setq org-download-image-dir org-download-path-last-dir)
  )

(map! :leader
      :desc "Insert a screenshot"
      "i s" 'org-download-screenshot
      :desc "Insert image from clipboard"
      "i p" 'org-download-clipboard
      "i P" 'org-download-clipboard-basename)
