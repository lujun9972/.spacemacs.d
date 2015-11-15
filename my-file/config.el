(use-package dired
  :defer t
  :init
  (require 'dired-x)
  :config
  ;; 默认情况下,! (dired-do-shell-command)不知道如何打开某些文件类型. 为它增加一些打开方式
  (setq dired-guess-shell-alist-user
        (list
         (list "\\.pdf$" "Foxit Reader")
         (list "\\.rm$" "smplayer")
         (list "\\.rmvb$" "smplayer")
         (list "\\.avi$" "smplayer")
         (list "\\.asf$" "smplayer")
         (list "\\.wmv$" "smplayer")
         (list "\\.htm$" "w3m")
         (list "\\.html$" "w3m")
         (list "\\.mpg$" "smplayer")))
  ;; 默认情况下,在Dired中打开其他目录,会打开一个新的Dired Buffer. 通过启用dired-find-alternate-file函数会用新的Dired Buffer代替老的Dired Buffer)
  (put 'dired-find-alternate-file 'disabled nil)
  ;; 自动刷新Dired-mode
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)
  (add-hook 'dired-mode-hook 'auto-revert-mode)
  ;; 隐藏某些文件
  (setq dired-omit-files "^\\..*$\\|^\\.$")
  (dired-omit-mode 1)                     ;在dired中可以通过C-x M-o开切换是否隐藏显示
  ;; if it is not Windows, use the following listing switches
  (when (not (eq system-type 'windows-nt))
    (setq dired-listing-switches "-lha --group-directories-first"))
  (setq
   dired-dwim-target t            ; 若在其他window有一个Dired buffer,则该Dired buffer的目录为Rename/Copy的默认地址
   dired-recursive-copies 'always         ; "always" means no asking
   dired-recursive-deletes 'top           ; "top" means ask once for top level directory
   dired-listing-switches "-lha"          ; human-readable listing
   delete-by-moving-to-trash t            ; 删除时放入回收站中 
   dired-x-hands-off-my-keys nil ;使用dired-x-find-file系列命令替代find-file命令,替代后,使用C-u C-x C-f会自动用光标所在的文本作为猜测的文件名
   )
  (use-package wdired
    :defer t
    :config
    (setq wdired-allow-to-change-permissions t)   ; allow to edit permission bits
    (setq wdired-allow-to-redirect-links)     ; allow to edit symlinks
    ))
