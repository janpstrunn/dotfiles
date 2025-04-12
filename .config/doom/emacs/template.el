;; Templates

(setq org-capture-directory "~/org/agenda/")

;; Org Capture
(setq org-capture-templates
      '(("t" "Todo" entry
         (file+headline (concat org-capture-directory "inbox.org") "Inbox")
         "* TODO %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?")
        ("e" "Event" entry
         (file+headline (concat org-capture-directory "calendar.org") "Events")
         "* %^{Event}\n%^{Schedule}T\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?")
        ("d" "Deadline" entry
         (file+headline (concat org-capture-directory "calendar.org") "Deadlines")
         "* TODO %^{Task}\nDEADLINE: %^{Deadline}T\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?")
        ("s" "Schedule" entry
         (file+headline (concat org-capture-directory "calendar.org") "Deadlines")
         "* TODO %^{Task}\nSCHEDULE: %^{Schedule}T\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?")
        ("p" "Project" entry
         (file+headline (concat org-capture-directory "project.org") "Projects")
         "* PROJ %^{Project name}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n** TODO %?")
        ("i" "Idea" entry
         (file+headline (concat org-capture-directory "ideas.org") "Ideas")
         "** IDEA %^{Idea}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?")
        ("c" "Contact" entry (file "../templates/contact.org")
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
           (file "../templates/source.org")
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n:ROAM_REFS:%^{Source}\n")
           :unnarrowed t)
          ("e" "encrypted" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org.gpg" "#+title: ${title}\n")
           :unnarrowed t)
          ("l" "library" plain
           (file "../templates/library.org")
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t))))

(after! org-roam-dailies
  (setq org-roam-dailies-directory "~/org/journal/")
  (setq org-roam-dailies-capture-templates
        `(("t" "today" plain (file "../templates/daily.org")
           :target (file+head "daily/%<%Y-%m-%d>.org" "#+title: Day: %<%Y-%m-%d>\n"))

          ("d" "default" plain "- %<%H:%M> %?"
           :if-new (file+head+olp "daily/%<%Y-%m-%d>.org" "#+title: Default: %<%Y-%m-%d>\n" ("Journal")))

          ("w" "weekly" plain (file "../templates/weekly.org")
           :if-new (file+head "weekly/%<%Y-W%V>.org" "#+title: Week: %<%Y-W%V>\n"))

          ("y" "yearly" plain (file "../templates/monthly.org")
           :if-new (file+head "%<%Y>.org" "#+title: Year: %<%Y>\n"))

          ("m" "monthly" plain (file "../templates/monthly.org")
           :if-new (file+head "monthly/%<%B>, %<%Y>.org" "#+title: Month: %<%B>, %<%Y>\n")))))
