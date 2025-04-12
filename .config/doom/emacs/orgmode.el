;; Org
(after! org
  (setq org-directory "~/org/"
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-ellipsis " ▼ "
        org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
        org-superstar-itembullet-alist '((?+ . ?➤) (?- . ?✦))
        org-log-done 'time
        org-hide-emphasis-markers t
        org-link-abbrev-alist
        '(("google" . "http://www.google.com/search?q=")
          ("arch-wiki" . "https://wiki.archlinux.org/index.php/")
          ("ddg" . "https://duckduckgo.com/?q=")
          ("wiki" . "https://en.wikipedia.org/wiki/"))
        org-table-convert-region-max-lines 20000
        org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "READY(r)" "ACTIVE(a)" "PROJ(p)" "|" "DONE(d!)" "WAIT(w)" "CANC(k)"))))

(defun my/org-files-with-tags (dir tags)
  "Return a list of Org files in DIR containing any of the TAGS in #+filetags:."
  (let ((files (directory-files-recursively dir "\\.org$"))
        (matched-files '()))
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file nil 0 300)  ;;
        (when (cl-loop for tag in tags
                       thereis (string-match-p (format "#\\+filetags:.*\\b%s\\b" tag)
                                               (buffer-string)))
          (push file matched-files))))
    matched-files))

;; Journals
(setq org-journal-dir "~/org/journal/"
      org-journal-date-prefix "#+title: "
      org-journal-time-prefix "* "
      org-journal-date-format "%B %d, %Y (%A) "
      org-journal-file-format "%Y-%m-%d.org")

(map! :leader
      :desc "Org babel tangle" "m B" #'org-babel-tangle)
