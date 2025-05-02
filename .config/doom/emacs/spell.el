;;; ../../dotfiles/.config/doom/emacs/spell.el -*- lexical-binding: t; -*-

(defun switch-to-english ()
  (interactive)
  (setq ispell-dictionary "en")
  (message "Switched to English dictionary"))

(defun switch-to-portuguese ()
  (interactive)
  (setq ispell-dictionary "pt_BR")
  (message "Switched to Portuguese dictionary"))

(global-set-key (kbd "C-c se") 'switch-to-english)
(global-set-key (kbd "C-c sp") 'switch-to-portuguese)

(remove-hook 'markdown-mode-hook #'spell-fu-mode)
(remove-hook 'org-mode-hook #'spell-fu-mode)
(remove-hook 'prog-mode-hook #'spell-fu-mode)
(remove-hook 'text-mode-hook #'spell-fu-mode)

(add-hook 'spell-fu-mode-hook
          (lambda ()
            (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en"))
            (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "pt_BR"))
            (spell-fu-dictionary-add
             (spell-fu-get-personal-dictionary "en-personal" "/home/janpstrunn/.aspell.pws"))))
