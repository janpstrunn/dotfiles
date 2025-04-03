(setq ob-mermaid-cli-path "~/.nix-profile/bin/mmdc")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((mermaid . t)
   (scheme . t)
   (your-other-langs . t)))
