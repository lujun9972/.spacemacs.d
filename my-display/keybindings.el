;; Ctrl+鼠标滚轮，改变字体大小
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

;; 默认M-上下,可以修改透明度
(global-set-key (kbd "<M-down>") (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
(global-set-key (kbd "<M-up>") (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-0") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))
