(use-package org-roam
  :ensure t
  :demand t  ;; Ensure org-roam is loaded by default
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-database-connector 'sqlite-builtin)
  (org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))
  (org-roam-directory "~/org/roam")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n I" . org-roam-node-insert-immediate)
         ("C-c n p" . my/org-roam-find-project)
         ("C-c n t" . my/org-roam-capture-task)
         ("C-c n b" . my/org-roam-capture-inbox)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c a" . org-roam-dailies-map)
  :config
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory t))
  (require 'org-roam-dailies)  ;; Ensure keymap is available
  (org-roam-db-autosync-mode))

(defun org-roam-node-insert-immediate (arg &rest args)
  "Insert an org-roam node immediately upon capture."
  (interactive "P")
  (let ((args (push arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun my/org-roam-filter-by-tag (tag-name)
  "Filter org-roam nodes by TAG-NAME."
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
  "List org-roam files that match a specific TAG-NAME."
  (mapcar #'org-roam-node-file
          (seq-filter (my/org-roam-filter-by-tag tag-name)
                      (org-roam-node-list))))

(defun my/org-roam-refresh-agenda-list ()
  "Refresh the org-agenda list with notes tagged 'Project'."
  (interactive)
  (setq org-agenda-files (my/org-roam-list-notes-by-tag "Project")))

;; Build agenda list when the session starts
(my/org-roam-refresh-agenda-list)

(defun my/org-roam-project-finalize-hook ()
  "Add the captured project file to org-agenda-files if capture was successful."
  (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))

(defun my/org-roam-find-project ()
  "Find or create a project node tagged with 'Project'."
  (interactive)
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
  (org-roam-node-find
   nil
   nil
   (my/org-roam-filter-by-tag "Project")
   :templates
   '(("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
      :unnarrowed t))))

(defun my/org-roam-capture-inbox ()
  "Capture a note into the Inbox."
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* %?"
                                   :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))

(defun my/org-roam-capture-task ()
  "Capture a task, creating the project file if needed."
  (interactive)
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
  (org-roam-capture- :node (org-roam-node-read nil (my/org-roam-filter-by-tag "Project"))
                     :templates '(("p" "project" plain "** TODO %?"
                                   :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                                          "#+title: ${title}\n#+category: ${title}\n#+filetags: Project"
                                                          ("Tasks"))))))

(defun my/org-roam-copy-todo-to-today ()
  "Refile a completed TODO to today's daily capture."
  (interactive)
  (let ((org-refile-keep t)
        (org-roam-dailies-capture-templates
         '(("t" "tasks" entry "%?"
            :if-new (file+head+olp "%<%Y-%m-%d>.org"
                                   "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
        today-file pos)
    (save-window-excursion
      (org-roam-dailies--capture (current-time) t)
      (setq today-file (buffer-file-name))
      (setq pos (point)))
    (unless (equal (file-truename today-file)
                   (file-truename (buffer-file-name)))
      (org-refile nil nil (list "Tasks" today-file nil pos)))))

(add-to-list 'org-after-todo-state-change-hook
             (lambda ()
               (when (equal org-state "DONE")
                 (my/org-roam-copy-todo-to-today))))

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
        `(("t" "daily" plain (file "~/org/templates/daily.org")
           :target (file+head "daily/%<%Y-%m-%d>.org" "#+title: Daily: %<%Y-%m-%d>\n"))

          ("d" "default" plain "** %<%I:%M %p> %?"
           :if-new (file+head+olp "daily/%<%Y-%m-%d>.org" "#+title: Default: %<%Y-%m-%d>\n" ("Journal")))

          ("w" "weekly" plain (file "~/org/templates/weekly.org")
           :if-new (file+head "weekly/%<%Y-W%U>.org" "#+title: Week: %<%Y-W%U>\n"))

          ("y" "yearly" plain (file "~/org/templates/monthly.org")
           :if-new (file+head "%<%Y>.org" "#+title: Year: %<%Y>\n"))

          ("m" "monthly" plain (file "~/org/templates/monthly.org")
           :if-new (file+head "monthly/%<%B>, %<%Y>.org" "#+title: Month: %<%B>, %<%Y>\n")))))

(defun org-roam-switch-db (&optional arg silent)
  "Switch to a different org-roam database, arg"
  (interactive)
  (when (not arg)
    (setq full-org-roam-db-list nil)

    (setq full-org-roam-db-list (directory-files "~/org" t "\\.[p,s]$"))
    (dolist (item full-org-roam-db-list)
      (setq full-org-roam-db-list
            (append (directory-files item t "\\.[p,s]$") full-org-roam-db-list)))

    (setq full-org-roam-db-list-pretty (list))
    (dolist (item full-org-roam-db-list)
      (setq full-org-roam-db-list-pretty
            (append (list
                     (replace-regexp-in-string (concat "\\/home\\/" user-username "\\/org\\/") "" item)) full-org-roam-db-list-pretty)))

    (setq org-roam-db-choice (completing-read "Select org roam database: "
                                              full-org-roam-db-list-pretty nil t)))
  (when arg
    (setq org-roam-db-choice arg))

  (setq org-roam-directory (file-truename (concat "~/org/" org-roam-db-choice "/Notes"))
        org-roam-db-location (file-truename (concat "~/org/" org-roam-db-choice "/Notes/org-roam.db"))
        org-directory (file-truename (concat "~/org/" org-roam-db-choice "/Notes")))
  (when (not silent))

  (setq mode-line-misc-info '((which-function-mode
                               (which-func-mode
                                ("" which-func-format " ")))
                              ("" so-long-mode-line-info)
                              (global-mode-string
                               ("" global-mode-string))
                              " "
                              org-roam-db-choice)
        )

  (org-roam-db-sync)

  (message (concat "Switched to " org-roam-db-choice " org-roam database!")))

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(setq org-roam-completion-everywhere t)
