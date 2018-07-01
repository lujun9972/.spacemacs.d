;;开启imenu菜单,可以通过Index菜单来跳转到各个defun的地方
;; (add-hook 'prog-mode-hook 'imenu-add-menubar-index)
(set-default 'imenu-auto-rescan t)

;; 配置vc-git在win环境下，用gbk编码格式进行提交
(when (member system-type '(ms-dos windows-nt))
  (setq vc-git-commits-coding-system 'gbk))

;; 保存时,检查文件是否包含#!,若包含则给文件添加执行权限
(defun mark-executable-when-save ()
  (add-hook 'after-save-hook
			'executable-make-buffer-file-executable-if-script-p nil t))
(add-hook 'prog-mode-hook #'mark-executable-when-save)

(setq-default grep-scroll-output t)		;随着grep输出结果的增多,buffer自动滚动

;; diff-hl高亮显示未提交的代码
;; (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
;; (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)

;; 设置compile
(setq compilation-always-kill t)        ;编译时,若有编译窗口,则直接关闭掉,不需要询问
(setq compilation-auto-jump-to-first-error t) ;编译出错,则自动跳转到第一个错误提示处
(setq compilation-scroll-output t)            ;自动滚动编译输出
;; 若编译无错误,则自动关闭compilation窗口
(defun compilation-abnormally-exit-message-p (exit-message)
  (if (string-match-p "exited abnormally" exit-message)
      t
    nil))

(defun compilation-mode-buffer-p (buf)
  "判断buffer是否为compilation-mode

  它与`compilation-buffer-p'不同之处在于`compilation-buffer-p'会对compilation-mode的子mode也返回t,而该函数返回nil"
  (eq 'compilation-mode (buffer-local-value 'major-mode (get-buffer buf))))

(defun compilation-kill-buffer-when-compile-success(buf exit-message)
  (tooltip-show exit-message)
  (when  (and (not (compilation-abnormally-exit-message-p exit-message))
              (compilation-mode-buffer-p buf))
    (kill-buffer buf)))
(add-to-list 'compilation-finish-functions #'compilation-kill-buffer-when-compile-success)

;; 记录上次编译失败时的compilation-buffer
(defvar last-fail-compilation-buffer nil
  "上一次编译失败的compliation buffer")

(defun log-last-fail-compliation-buffer (buf msg)
  "记录下上一次编译失败的compliation buffer到`last-fail-compilation-buffer'中"
  (if (compilation-abnormally-exit-message-p msg)
      (setq last-fail-compilation-buffer buf)
    (setq last-fail-compilation-buffer nil)))

(add-to-list 'compilation-finish-functions #'log-last-fail-compliation-buffer)

;; 使用" #!/usr/bin/env 解析器" 的形式
(setq executable-prefix-env t)
