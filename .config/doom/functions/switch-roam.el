(defun org-roam-switch-db (&optional arg silent)
  "Switch to a different org-roam database, arg"
  (interactive)
  (when (not arg)
    (setq full-org-roam-db-list nil)

    (setq full-org-roam-db-list (directory-files "~/org/roam" t "\\.[p,s]$"))
    (dolist (item full-org-roam-db-list)
      (setq full-org-roam-db-list
            (append (directory-files item t "\\.[p,s]$") full-org-roam-db-list)))

    (setq full-org-roam-db-list-pretty (list))
    (dolist (item full-org-roam-db-list)
      (setq full-org-roam-db-list-pretty
            (append (list
                     (replace-regexp-in-string (concat "\\/home\\/" user-username "\\/org/roam\\/") "" item)) full-org-roam-db-list-pretty)))

    (setq org-roam-db-choice (completing-read "Select org roam database: "
                                              full-org-roam-db-list-pretty nil t)))
  (when arg
    (setq org-roam-db-choice arg))

  (setq org-roam-directory (file-truename (concat "~/org/roam/" org-roam-db-choice "/"))
        org-roam-db-location (file-truename (concat "~/org/roam/" org-roam-db-choice "/org-roam.db")))
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
