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
        '((sequence "TODO(t)" "NEXT(n)" "READY(r)" "ACTIVE(a)" "|" "DONE(d!)" "WAIT(w)" "CANC(k)"))))

  ;; Agenda
  (after! org
    (setq org-agenda-files '("~/org/roam/Inbox.org" "~/org/projects.org" "~/org/agenda.org"))

    (setq
     org-fancy-priorities-list '("[A]" "[B]" "[C]")
     ;; org-fancy-priorities-list '("❗" "[B]" "[C]")
     ;; org-fancy-priorities-list '("🟥" "🟧" "🟨")
     org-priority-faces
     '((?A :foreground "#fc2020" :weight bold)
       (?B :foreground "#fcae5f" :weight bold)
       (?C :foreground "#f9fc5f" :weight bold))
     org-agenda-block-separator 8411

     org-agenda-custom-commands
     '(("v" "A better agenda view"
        ((tags "PRIORITY=\"A\""
               ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                (org-agenda-overriding-header "High-priority unfinished tasks:")))
         (tags "PRIORITY=\"B\""
               ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                (org-agenda-overriding-header "Medium-priority unfinished tasks:")))
         (tags "PRIORITY=\"C\""
               ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                (org-agenda-overriding-header "Low-priority unfinished tasks:")))
         (tags "customtag"
               ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                (org-agenda-overriding-header "Tasks marked with customtag:")))
         (agenda "")
         (alltodo "")))

       ("d" "Dashboard"
        ((agenda "" ((org-deadline-warning-days 7)))
         (todo "NEXT" ((org-agenda-overriding-header "Next Tasks")))
         (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

       ("n" "Next Tasks"
        ((todo "NEXT" ((org-agenda-overriding-header "Next Tasks")))))

       ("W" "Work Tasks"
        ((tags-todo "+work")))

       ("w" "Workflow Status"
        ((todo "WAIT" ((org-agenda-overriding-header "Waiting on External")))
         (todo "REVIEW" ((org-agenda-overriding-header "In Review")))
         (todo "PLAN" ((org-agenda-overriding-header "In Planning") (org-agenda-todo-list-sublevels nil)))
         (todo "BACKLOG" ((org-agenda-overriding-header "Project Backlog") (org-agenda-todo-list-sublevels nil)))
         (todo "READY" ((org-agenda-overriding-header "Ready for Work")))
         (todo "ACTIVE" ((org-agenda-overriding-header "Active Projects")))
         (todo "COMPLETED" ((org-agenda-overriding-header "Completed Projects")))
         (todo "CANC" ((org-agenda-overriding-header "Cancelled Projects"))))))))

  ;; Journals
  (setq org-journal-dir "~/org/journal/"
        org-journal-date-prefix "* "
        org-journal-time-prefix "** "
        org-journal-date-format "%B %d, %Y (%A) "
        org-journal-file-format "%Y-%m-%d.org")

  (map! :leader
        :desc "Org babel tangle" "m B" #'org-babel-tangle)
