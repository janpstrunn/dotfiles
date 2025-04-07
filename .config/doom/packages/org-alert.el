(use-package org-alert
  :ensure t)
(setq alert-default-style 'libnotify)
(setq org-alert-interval 300
      org-alert-notify-cutoff 60
      org-alert-notification-title "Org Agenda"
      org-alert-notify-after-event-cutoff 10)
(org-alert-enable)
