(define-globalized-minor-mode global-rainbow-mode rainbow-mode
  (lambda ()
    (when (not (memq major-mode
                     (list 'org-agenda-mode)))
      (rainbow-mode 1))))
(global-rainbow-mode 1 )
