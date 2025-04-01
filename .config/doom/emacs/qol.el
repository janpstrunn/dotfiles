;; Good for Syncthing
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-use-notify t)

;; Inline Images
(setq org-startup-with-inline-images t
      org-image-actual-width nil)

;; Mini Buffer
(setq-default truncate-lines t)
(setq completions-format 'one-column)
(setq completions-detailed t)
(setq truncate-partial-width-windows nil)
