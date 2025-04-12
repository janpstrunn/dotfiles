(add-hook 'find-file-hook #'eglot-ensure)
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '(text-mode . ("harper-ls" "--stdio"))))
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '(markdown-mode . ("harper-ls" "--stdio"))))
