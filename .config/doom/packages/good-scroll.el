(setq scroll-conservatively 101)

(good-scroll-mode 1)
(setq good-scroll-duration 0.4
      good-scroll-step 270
      good-scroll-render-rate 0.03)

(global-set-key (kbd "<next>") #'good-scroll-up-full-screen)
(global-set-key (kbd "<prior>") #'good-scroll-down-full-screen)

(setq scroll-margin 30)
(setq hscroll-margin 10)
