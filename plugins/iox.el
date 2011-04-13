(defun iox ()
  "indent for one line xml file."
  (interactive)
  (mark-whole-buffer)
  (replace-string "><" ">
<")
  (mark-whole-buffer)
  (indent-region (region-beginning) (region-end))
  )

(provide 'iox)
;;; iox.el ends here