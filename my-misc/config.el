;;开启hl-line模式
;; (global-hl-line-mode 1)
;; 让 Emacs 可以直接打开和显示图片。
(auto-image-file-mode)
;;开启fileset功能
;; (filesets-init)
;;开启winner-mode,可以undo /redo 对windows的操作 C-c <- 或 C-c ->
(winner-mode 1)
;;开启windmove,使用shift-方向键切换window
(windmove-default-keybindings 'shift)
;; 透明处理压缩文件
(auto-compression-mode 1)
;; 配置ediff
(setq ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)
;; 为防止不小心按到C-c C-x,在退出Emacs前需要确认
(setq confirm-kill-emacs (lambda (prompt) (y-or-n-p-with-timeout "是否退出Emacs:(" 10 "y")))

;开启自动上传功能
(use-package autoftp
  :config
  (setq autoftp-local-remote-root-alist '(("e:/git-svn/server/" "cnaps2" "10.8.6.10" "")
                                          ("/cygdrive/e/git-svn/server/" "cnaps2" "10.8.6.10" "")
                                          ("~/trunk/" "cnaps2" "10.8.6.10" "")
                                          ("d:/workcvs/cnaps2/server/trunk/" "cnaps2" "10.8.6.10" "")
                                          ("d:/workcvs/ibps/ibps" "ibpsusr" "10.8.6.10" ""))))
