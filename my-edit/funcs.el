(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(defun set-sentence-end-double-space ()
  "Set `sentence-end-double-space' according to how often the
  literal string \".  \" occurs in the current buffer."
  (make-local-variable 'sentence-end-double-space)
  (if (>= (* (count-matches "\\.  ") 1024)
          (* (buffer-size) sentence-end-double-space-threshold))
      (setq sentence-end-double-space t)
    (setq sentence-end-double-space nil)))
