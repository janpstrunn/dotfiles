(defun my-org-format-elisp-blocks ()
  (when (eq major-mode 'org-mode)
    (org-babel-map-src-blocks nil
      (when (string= lang "emacs-lisp")
        (org-edit-special)
        (indent-region (point-min) (point-max))
        (org-edit-src-exit)))))

(add-hook 'before-save-hook #'my-org-format-elisp-blocks)
