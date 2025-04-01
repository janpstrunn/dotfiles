;; User Variables
(setq user-full-name "Janpstrunn"
      user-mail-address "janpstrunn@disroot.org")
(setq user-username "janpstrunn")
(setq system-wm-type "wayland") ; wayland or x11?

;; Bookmark Directory
(setq bookmark-default-file "~/.config/doom/bookmarks")

;; Fonts
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14)
      doom-variable-pitch-font (font-spec :family "Ubuntu Nerd Font" :size 14)
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 20))

;; Theming
;; (setq doom-theme 'doom-challenger-deep)
(setq custom-theme-directory "~/.config/doom/themes")
(setq doom-theme 'elegant-vagrant)
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

;; camelCase
(global-subword-mode 1)

;; Relative Lines
(setq display-line-numbers-type 'relative)
