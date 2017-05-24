(defun set-info-keys()
  ""
  (interactive)
  (local-set-key (kbd "<mouse-4>") 'mwheel-scroll )
  (local-set-key (kbd "<mouse-5>") 'mwheel-scroll ) )

;; 使用C-h C-f查看函数的源代码
(defun find-function-view-mode (fun)
  (interactive (find-function-read))
  (find-function-do-it fun nil 'switch-to-buffer)
  (view-mode 1))
