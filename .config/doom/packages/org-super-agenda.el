(setq org-super-agenda-groups
      '(;; Each group has an implicit boolean OR operator between its selectors.
        (:name " Overdue "  ; Optionally specify section name
         :scheduled past
         :order 2
         :face 'error)

        (:name "Personal "
         :and(:file-path "personal.p" :not (:tag "event"))
         :order 3)

        (:name "Family "
         :and(:file-path "Family.s" :not (:tag "event"))
         :order 3)

        (:name "Teaching "
         :and(:file-path "Teaching.p" :not (:tag "event"))
         :order 3)

        (:name "Gamedev "
         :and(:file-path "Gamedev.s" :not (:tag "event"))
         :order 3)

        (:name "Youtube "
         :and(:file-path "Producer.p" :not (:tag "event"))
         :order 3)

        (:name "Music "
         :and(:file-path "Bard.p" :not (:tag "event"))
         :order 3)

        (:name "Storywriting "
         :and(:file-path "Stories.s" :not (:tag "event"))
         :order 3)

        (:name "Writing "
         :and(:file-path "Author.p" :not (:tag "event"))
         :order 3)

        (:name "Learning "
         :and(:file-path "Knowledge.p" :not (:tag "event"))
         :order 3)

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
