;; Templates

(setq org-capture-directory "~/org/agenda/")
(setq org-capture-template-directory "~/.config/doom/templates/")

;; Org Capture
(setq org-capture-templates
      '(("t" "Todo" entry
         (file+headline "~/org/agenda/inbox.org" "Inbox")
         "* TODO %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?")
        ("e" "Event" entry
         (file+headline "~/org/agenda/calendar.org" "Events")
         "* %^{Event}\n%^{Schedule}T\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?")
        ("d" "Deadline" entry
         (file+headline "~/org/agenda/calendar.org" "Deadlines")
         "* TODO %^{Task}\nDEADLINE: %^{Deadline}T\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?")
        ("s" "Schedule" entry
         (file+headline "~/org/agenda/calendar.org" "Deadlines")
         "* TODO %^{Task}\nSCHEDULE: %^{Schedule}T\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?")
        ("p" "Project" entry
         (file+headline "~/org/agenda/project.org" "Projects")
         "* PROJ %^{Project name}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n** TODO %?")
        ("i" "Idea" entry
         (file+headline "~/org/agenda/ideas.org" "Ideas")
         "** IDEA %^{Idea}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?")
        ("c" "Contact" entry (file "~/.config/doom/templates/contact.org")
         (file+headline "~/org/contacts.org" "Contacts"))))

(after! org-roam
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("t" "timestamped entry" entry "- %<%H:%M> %?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("s" "source" plain
           (file "~/.config/doom/templates/source.org")
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n:ROAM_REFS:%^{Source}\n")
           :unnarrowed t)
          ("e" "encrypted" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org.gpg" "#+title: ${title}\n")
           :unnarrowed t)
          ("l" "library" plain
           (file "~/.config/doom/templates/library.org")
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t))))

(after! org-roam-dailies
  (setq org-roam-dailies-directory "~/org/journal/")
  (setq org-roam-dailies-capture-templates
        `(("t" "today" plain (file "~/.config/doom/templates/daily.org")
           :target (file+head "daily/%<%Y-%m-%d>.org" "#+title: Day: %<%Y-%m-%d>\n"))

          ("d" "default" plain "- %<%H:%M> %?"
           :if-new (file+head+olp "daily/%<%Y-%m-%d>.org" "#+title: Default: %<%Y-%m-%d>\n" ("Journal")))

          ("w" "weekly" plain (file "~/.config/doom/templates/weekly.org")
           :if-new (file+head "weekly/%<%Y-W%V>.org" "#+title: Week: %<%Y-W%V>\n"))

          ("y" "yearly" plain (file "~/.config/doom/templates/monthly.org")
           :if-new (file+head "%<%Y>.org" "#+title: Year: %<%Y>\n"))

          ("m" "monthly" plain (file "~/.config/doom/templates/monthly.org")
           :if-new (file+head "monthly/%<%B>, %<%Y>.org" "#+title: Month: %<%B>, %<%Y>\n")))))
