(defun my-trim-trailing-whitespace ()
  "Delete trailing whitespace at the end of all lines on save."
  (add-hook 'before-save-hook #'delete-trailing-whitespace))
(my-trim-trailing-whitespace)
