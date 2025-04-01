(add-load-path! "~/path/to/mu4e")
(require 'mu4e)
(require 'mu4e-contrib)
(require 'mu4e-actions)

(after! mu4e
  (setq mu4e-modeline-support nil)
  (setq mu4e-sent-folder (lambda (msg) (concat "/" (nth 1 (split-string (mu4e-message-field msg :maildir) "/" )) "/Sent")))
  (setq mu4e-drafts-folder (lambda (msg) (concat "/" user-mail-address "/Drafts")))
  (setq mu4e-trash-folder (lambda (msg) (concat "/" (nth 1 (split-string (mu4e-message-field msg :maildir) "/" )) "/Trash")))
  (setq mu4e-refile-folder (lambda (msg) (concat "/" (nth 1 (split-string (mu4e-message-field msg :maildir) "/" )) "/Folders/" (completing-read "Refile msg to: " (directory-files (concat "~/.mail/" (nth 1 (split-string (mu4e-message-field msg :maildir) "/" )) "/Folders"))))))

  (setq mu4e-index-lazy-check t)
  (setq mu4e-index-cleanup t)
  (setq mu4e-update-interval 120)
  (mu4e-alert-enable-notifications)

  (define-key mu4e-main-mode-map (kbd "<SPC>") #'doom/leader)
  (define-key mu4e-headers-mode-map (kbd "<SPC>") #'doom/leader)
  (define-key mu4e-view-mode-map (kbd "<SPC>") #'doom/leader)

  (define-key mu4e-main-mode-map (kbd "g g") #'evil-goto-first-line)
  (define-key mu4e-main-mode-map (kbd "G") #'evil-goto-line)
  (define-key mu4e-main-mode-map (kbd "h") #'evil-backward-char)
  (define-key mu4e-main-mode-map (kbd "l") #'evil-forward-char)
  (define-key mu4e-main-mode-map (kbd "w") #'evil-forward-word-begin)

  (unbind-key "g" mu4e-headers-mode-map)
  (define-key mu4e-headers-mode-map (kbd "g g") #'evil-goto-first-line)
  (define-key mu4e-headers-mode-map (kbd "G") #'evil-goto-line)
  (define-key mu4e-headers-mode-map (kbd "h") #'evil-backward-char)
  (define-key mu4e-headers-mode-map (kbd "l") #'evil-forward-char)
  (define-key mu4e-headers-mode-map (kbd "w") #'evil-forward-word-begin)

  (unbind-key "g" mu4e-view-mode-map)
  (define-key mu4e-view-mode-map (kbd "g g") #'evil-goto-first-line)
  (define-key mu4e-view-mode-map (kbd "G") #'evil-goto-line)
  (define-key mu4e-view-mode-map (kbd "h") #'evil-backward-char)
  (define-key mu4e-view-mode-map (kbd "l") #'evil-forward-char)
  (define-key mu4e-view-mode-map (kbd "w") #'evil-forward-word-begin)

  (map! :map 'mu4e-main-mode-map :desc "Jump to maildir" :ge "J" #'mu4e-search-maildir)
  (map! :map 'mu4e-main-mode-map :desc "Next line" :ge "j" #'evil-next-visual-line)
  (map! :map 'mu4e-main-mode-map :desc "Prev line" :ge "k" #'evil-previous-visual-line)

  (map! :map 'mu4e-headers-mode-map :desc "Jump to maildir" :ge "J" #'mu4e-search-maildir)
  (map! :map 'mu4e-headers-mode-map :desc "Next line" :ge "j" #'evil-next-visual-line)
  (map! :map 'mu4e-headers-mode-map :desc "Prev line" :ge "k" #'evil-previous-visual-line)
  (map! :map 'mu4e-headers-mode-map :desc "Next char" :ge "l" #'evil-forward-char)
  (map! :map 'mu4e-headers-mode-map :desc "Update mail and index" :ge "U" #'mu4e-update-mail-and-index)
  (map! :map 'mu4e-headers-mode-map :desc "Compose reply" :ge "r" #'mu4e-compose-reply)
  (map! :map 'mu4e-headers-mode-map :desc "Archive message" :ge "e" #'mu4e-headers-mark-for-refile)

  (map! :map 'mu4e-view-mode-map :desc "Jump to maildir" :ge "J" #'mu4e-search-maildir)
  (map! :map 'mu4e-view-mode-map :desc "Next line" :ge "j" #'evil-next-visual-line)
  (map! :map 'mu4e-view-mode-map :desc "Prev line" :ge "k" #'evil-previous-visual-line)
  (map! :map 'mu4e-view-mode-map :desc "Update mail and index" :ge "U" #'mu4e-update-mail-and-index)
  (map! :map 'mu4e-view-mode-map :desc "Compose reply" :ge "r" #'mu4e-compose-reply)
  (map! :map 'mu4e-view-mode-map :desc "Archive message" :ge "e" #'mu4e-view-mark-for-refile)

  (add-to-list 'mu4e-header-info-custom
               '(:maildir-folder-no-account .
                 ( :name "Maildir folder without account"  ;; long name, as seen in the message-view
                         :shortname "Folder"           ;; short name, as seen in the headers view
                         :help "Name of the subfolder without the maildir" ;; tooltip
                         :function (lambda (msg) (substring (mu4e-message-field msg :maildir) (+ 2 (length (nth 1 (split-string (mu4e-message-field msg :maildir) "/" )))))))))
  (add-to-list 'mu4e-bookmarks
               '( :name  "Unified inbox"
                  :query "maildir://.*/INBOX/"
                  :key   ?i))
  (setq mu4e-headers-fields
        '((:account-stripe . 1)
          (:account . 25)
          (:human-date . 12)
          (:flags . 6)
          (:from-or-to . 20)
          (:maildir-folder-no-account . 30)
          (:subject)))

  (add-hook 'mu4e-compose-mode-hook #'(lambda () (auto-save-mode -1)))
  (setq mu4e-sent-messages-behavior 'delete)

  (setq sendmail-program "~/.nix-profile/bin/msmtp")
  (setq send-mail-function 'smtpmail-send-it)
  (setq message-sendmail-f-is-evil t)
  (setq message-sendmail-extra-arguments '("--read-envelope-from"))
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (if (file-exists-p "~/.emacs.d/mu4e-private.el") (load! "~/.emacs.d/mu4e-private.el"))

  ;; https://emacs.stackexchange.com/questions/3061/how-to-stop-mu4e-from-inserting-line-breaks
  (defun no-auto-fill ()
    "Turn off auto-fill-mode."
    (auto-fill-mode -1))

  (defun no-org-msg-mode ()
    "Disable org-msg-mode since it doesn't respect multiline emails"
    (org-msg-mode 0))

  (add-hook 'mu4e-compose-mode-hook #'no-auto-fill)
  (add-hook 'mu4e-compose-pre-hook #'no-org-msg-mode)

  (mu4e--start) ;; start mu4e silently

  )
