(after! counsel
  (map! :leader
        "." #'zoxide-travel
        ">" #'zoxide-find-file))
