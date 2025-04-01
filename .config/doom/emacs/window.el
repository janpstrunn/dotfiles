(defun prefer-horizontal-split ()
  (set-variable 'split-height-threshold nil t)
  (set-variable 'split-width-threshold 40 t))
(add-hook 'markdown-mode-hook 'prefer-horizontal-split)

(map! :leader
      :desc "Clone indirect buffer other window" "b c" #'clone-indirect-buffer-other-window)

(setq resize-mini-windows nil)

(bind-key* "M-j" #'evil-window-down)
(bind-key* "M-k" #'evil-window-up)
(bind-key* "M-h" #'evil-window-left)
(bind-key* "M-l" #'evil-window-right)
(bind-key* "C-q" #'evil-window-delete)
(bind-key* "M-q" #'kill-current-buffer)
(bind-key* "M-q" #'kill-current-buffer)
(bind-key* "M-w" #'+workspace/close-window-or-workspace)
(bind-key* "M-n" #'next-buffer)
(bind-key* "M-p" #'previous-buffer)
(bind-key* "M-z" #'+vterm/toggle)
(bind-key* "M-e" #'+eshell/toggle)
(bind-key* (kbd "M-<return>") #'+vterm/here)
(bind-key* (kbd "M-E") #'+eshell/here)
