(add-hook 'after-init-hook 'global-company-mode)  ;; Enable company-mode globally
(add-hook 'org-mode-hook (lambda ()  ;; Enable completion in Org mode
                           (setq-local company-backends '((company-capf))))  ;; Add company-capf to company-backends
          )
