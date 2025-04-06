;;; ../../dotfiles/.config/doom/emacs/org-agenda.el -*- lexical-binding: t; -*-

(use-package all-the-icons
  :if (display-graphic-p))

(custom-set-faces!
  '(org-agenda-date :inherit outline-1 :height 1.15)
  '(org-agenda-date-today :inherit outline-2 :height 1.15)
  '(org-agenda-date-weekend :inherit outline-1 :height 1.15)
  '(org-agenda-date-weekend-today :inherit outline-2 :height 1.15)
  '(org-super-agenda-header :inherit custom-button :weight bold :height 1.05))

(setq org-agenda-category-icon-alist
      `(("Graduation" ,(list (all-the-icons-faicon "graduation-cap" :height 0.8)) nil nil :ascent center)
        ("Family" ,(list (all-the-icons-faicon "home" :v-adjust 0.005)) nil nil :ascent center)
        ("Books" ,(list (all-the-icons-faicon "book" :height 0.9)) nil nil :ascent center)
        ("Personal" ,(list (all-the-icons-material "person" :height 0.9)) nil nil :ascent center)
        ("Inbox" ,(list (all-the-icons-material "inbox" :height 0.9)) nil nil :ascent center)
        ))

;; (setq org-agenda-current-time-string "")
;; (setq org-agenda-time-grid '((daily) () "" ""))

(setq org-agenda-prefix-format '(
                                 (agenda . "  %?-2i %t ")
                                 (todo . " %i %-12:c")
                                 (tags . " %i %-12:c")
                                 (search . " %i %-12:c")))

(defun org-agenda-switch-with-roam ()
  (interactive)
  (org-agenda-switch-to)
  (if (f-exists-p (concat (dir!) "/org-roam.db"))
      (org-roam-switch-db (replace-regexp-in-string (concat "\\/home\\/" user-username "\\/org\\/") "" (f-parent (dir!))) t))
  (if (f-exists-p (concat (f-parent (dir!)) "/org-roam.db"))
      (org-roam-switch-db (replace-regexp-in-string (concat "\\/home\\/" user-username "\\/org\\/") "" (f-parent (f-parent (dir!)))) t))
  (org-roam-olivetti-mode)
  )

(map!
 :map evil-org-agenda-mode-map
 :after org-agenda
 :nvmeg "<RET>" #'org-agenda-switch-with-roam
 :nvmeg "<return>" #'org-agenda-switch-with-roam)
(map!
 :map org-agenda-mode-map
 :after org-agenda
 :nvmeg "<RET>" #'org-agenda-switch-with-roam
 :nvmeg "<return>" #'org-agenda-switch-with-roam)

;; Agenda
(after! org
  (setq org-agenda-files
        (append (file-expand-wildcards "~/org/**/*.org")
                (my/org-files-with-tags "~/org/" '("Project" "Personal" "Work"))))

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

     ("n" "Now - Today's Due Tasks"
      ((agenda "" ((org-agenda-span 1)(org-deadline-warning-days 1)(org-agenda-start-day "+0d")))))

     ("d" "Dashboard"
      ((agenda "" ((org-deadline-warning-days 7)))
       (todo "NEXT" ((org-agenda-overriding-header "Next Tasks")))
       (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

     ("N" "Next Tasks"
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
