;; "Delete trailing whitespace at the end of all lines before saving."

(defun my-trim-trailing-whitespace ()
  (add-hook 'before-save-hook #'delete-trailing-whitespace))
(my-trim-trailing-whitespace)
