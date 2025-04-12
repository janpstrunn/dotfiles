(setq org-super-agenda-groups
      '(;; Each group has an implicit boolean OR operator between its selectors.
        (:name " Overdue "  ; Optionally specify section name
         :scheduled past
         :order 2
         :face 'error)

        (:name "Personal "
         :and(:file-path "personal.p" :not (:tag "event"))
         :order 3)

        (:name "Yggdrasil "
         :and(:file-path "yggdrasil.p" :not (:tag "event"))
         :order 3)

        (:name "Hephaestus "
         :and(:file-path "hephaestus.p" :not (:tag "event"))
         :order 3)

        (:name "Inbox "
         :tag "Inbox"
         :order 4)

        (:name "Project "
         :tag "Project"
         :order 4)

        (:name " Today "  ; Optionally specify section name
         :time-grid t
         :date today
         :scheduled today
         :order 1
         :face 'warning)

        ))

(org-super-agenda-mode t)

(map! :desc "Next line"
      :map org-super-agenda-header-map
      "j" 'org-agenda-next-line)

(map! :desc "Next line"
      :map org-super-agenda-header-map
      "k" 'org-agenda-previous-line)
