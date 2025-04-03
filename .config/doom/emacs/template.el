;; Templates

;; Org Capture
(setq org-capture-templates
      '(("t" "Todo" entry
         (file+headline "~/org/inbox.org" "Inbox")
         "* TODO %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?")
        ("e" "Event" entry
         (file+headline "~/org/calendar.org" "Events")
         "* %^{Event}\n%^{SCHEDULED}T\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:CONTACT: %(org-capture-ref-link \"~/org/contacts.org\")\n:END:\n%?")
        ("d" "Deadline" entry
         (file+headline "~/org/calendar.org" "Deadlines")
         "* TODO %^{Task}\nDEADLINE: %^{Deadline}T\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?")
        ("p" "Project" entry
         (file+headline "~/org/projects.org" "Projects")
         "* PROJ %^{Project name}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n** TODO %?")
        ("i" "Idea" entry
         (file+headline "~/org/ideas.org" "Ideas")
         "** IDEA %^{Idea}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?")
        ("c" "Contact" entry
         (file+headline "~/org/contacts.org" "Inbox")
         "* %^{Name}

:PROPERTIES:
:CREATED: %U
:CAPTURED: %a
:EMAIL: %^{Email}
:PHONE: %^{Phone}
:BIRTHDAY: %^{Birthday +1y}u
:LOCATION: %^{Address}
:LAST_CONTACTED: %U
:END:
\\ *** Communications
\\ *** Notes
%?")
        ("n" "Note" entry
         (file+headline "~/org/notes.org" "Inbox")
         "* [%<%Y-%m-%d %a>] %^{Title}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?"
         :prepend t)))


(after! org-roam
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("t" "timestamped entry" entry "%<%I:%M %p> %?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("e" "encrypted" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org.gpg" "#+title: ${title}\n")
           :unnarrowed t)
          ("l" "library" plain
           (file "~/org/templates/library.org")
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t))))

(after! org-roam-dailies
  (setq org-roam-dailies-directory "~/org/journal/")
  (setq org-roam-dailies-capture-templates
        `(("t" "today" plain (file "~/org/templates/daily.org")
           :target (file+head "daily/%<%Y-%m-%d>.org" "#+title: Daily: %<%Y-%m-%d>\n"))

          ("d" "default" plain "** %<%I:%M %p> %?"
           :if-new (file+head+olp "daily/%<%Y-%m-%d>.org" "#+title: Default: %<%Y-%m-%d>\n" ("Journal")))

          ("w" "weekly" plain (file "~/org/templates/weekly.org")
           :if-new (file+head "weekly/%<%Y-W%U>.org" "#+title: Week: %<%Y-W%U>\n"))

          ("y" "yearly" plain (file "~/org/templates/monthly.org")
           :if-new (file+head "%<%Y>.org" "#+title: Year: %<%Y>\n"))

          ("m" "monthly" plain (file "~/org/templates/monthly.org")
           :if-new (file+head "monthly/%<%B>, %<%Y>.org" "#+title: Month: %<%B>, %<%Y>\n")))))
