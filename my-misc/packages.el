;;; packages.el --- my-misc Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq my-misc-packages
    '(
      ;; package names go here
      (ido :location built-in)
      ;; smex
      (desktop :location built-in)
      ;; tabbar
      uimage
      keyfreq
      sr-speedbar
      ibuffer
      ibuffer-vc
      dictionary
      showkey
      dmenu
      start-menu
      clean-buffers
      verify-url
      (pocket-mode :location (recipe
                              :fetcher github
                              :repo "lujun9972/pocket-mode"))
      ))

;; List of packages to exclude.
(setq my-misc-excluded-packages '())

;; For each package, define a function my-misc/init-<package-name>
;;

;; 开启ido-mode
(defun my-misc/post-init-ido ()
  (use-package ido
    :config
    (ido-mode 'both)
    (setq ido-enable-prefix nil
          ido-enable-flex-matching t 
          ido-everywhere t
          ido-max-directory-size 100000
          ido-create-new-buffer 'always
          ;; Use the current window when visiting files and buffers with ido
          ido-default-file-method 'selected-window
          ido-default-buffer-method 'selected-window
          ido-use-filename-at-point nil
          ido-auto-merge-work-directories-length 0
          ido-use-virtual-buffers t)
    (add-to-list 'ido-ignore-directories "\\`\\.git/")
    (add-to-list 'ido-ignore-files "\\~\\`")
    (add-to-list 'ido-ignore-files "\\.doc\\`")
    ;; 若打开文件没有权限,自动使用sudo方式打开
    (defun alternate-current-file-as-root (&rest args)
      "以sudo方式打开当前buffer文件"
      (interactive)
      (let ((file (buffer-file-name)))
        (when (and file
                   (not (file-writable-p file))
                   (not (file-remote-p file))
                   (y-or-n-p-with-timeout "是否使用sudo方式打开当前文件" 10 "n"))
          (find-alternate-file (concat "/sudo::" file)))))
    (advice-add 'ido-find-file :after #'alternate-current-file-as-root)))

(defun my-misc/init-ido-ubiquitous ()
    (use-package ido-ubiquitous
      :ensure t
      :config
      (ido-ubiquitous-mode t)))

;; (defun my-misc/init-smex ()
;;   (use-package smex
;;     :ensure t
;;     :config
;;     (smex-initialize)
;;     :bind (("M-x" . smex)
;;            ("C-x x" . smex)
;;            ("M-X" . smex-major-mode-commands))))

(defun my-misc/post-init-desktop ()
  (use-package desktop
    :config
    (setq desktop-path '("."))
    ;; 为每个不同的系统定义不同的保存名称,这是为了使得同一台机器上不同环境间互补干扰
    (setq desktop-base-file-name (file-name-nondirectory (format "%s.emacs.desktop" system-type)))
    (setq desktop-auto-save-timeout 600)	;10分钟保存一次
    (desktop-save-mode 1)
    (toggle-save-place-globally 1)			;保存各buffer的光标位置
    (savehist-mode t)						;保存minibuffer的history
    ))

(defun my-misc/init-tabbar ()
  "Initialize my package"
  (use-package tabbar
    :config
    (tabbar-mode t)
    (defvar tabbar-buffer-name-group-alist
      '(("*tramp.+*" "hidden"))
      "根据名称的正则表达式来为tabbar提供分组信息

每个原始的car为buffer-name匹配的正则表达式, cdr为分组list")

    (defun my-tabbar-buffer-groups ()
      "tabbar分组函数

先根据`tabbar-buffer-name-group-alist'与buffer-name查找匹配的分组,否则使用`tabbar-buffer-groups'进行分组"
      (let ((match-rule (find-if (lambda (match-rule)
                                   (string-match-p (car match-rule) (buffer-name))) tabbar-buffer-name-group-alist)))
        (if match-rule
            (cdr match-rule)
          (tabbar-buffer-groups))))

    (setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)))

(defun my-misc/init-uimage ()
  (use-package uimage
    :ensure t))

;; 配置keyfreq用来记录按键频率,使用keyfre-show查看
(defun my-misc/init-keyfreq ()
  (use-package keyfreq
    :config
    (keyfreq-mode 1)
    (keyfreq-autosave-mode 1)))

;; (defun my-misc/init-sr-speedbar ()
;;   (use-package sr-speedbar
;;     :init
;;     (defalias 'ad-advised-definition-p #'ad-advice-p)
;;     :config
;;     (setq sr-speedbar-skip-other-window-p t)
;;     (setq sr-speedbar-right-side nil)))

(defun my-misc/init-ibuffer ()
  (use-package ibuffer
    :init
    (defun ibuffer-set-up-preferred-filters ()
      (ibuffer-vc-set-filter-groups-by-vc-root)
      (unless (eq ibuffer-sorting-mode 'filename/process)
        (ibuffer-do-sort-by-filename/process)))

    (add-hook 'ibuffer-hook 'ibuffer-set-up-preferred-filters)
    (setq-default ibuffer-show-empty-filter-groups nil)
    ;; Modify the default ibuffer-formats (toggle with `)
    (setq ibuffer-formats
          '((mark modified read-only vc-status-mini " "
                  (name 18 18 :left :elide)
                  " "
                  (size-h 9 -1 :right)
                  " "
                  (mode 16 16 :left :elide)
                  " "
                  filename-and-process)
            (mark modified read-only vc-status-mini " "
                  (name 18 18 :left :elide)
                  " "
                  (size-h 9 -1 :right)
                  " "
                  (mode 16 16 :left :elide)
                  " "
                  (vc-status 16 16 :left)
                  " "
                  filename-and-process)))

    (setq ibuffer-filter-group-name-face 'font-lock-doc-face)
    (global-set-key (kbd "C-x C-b") 'ibuffer)
    :defer t
    :config
    (fullframe ibuffer ibuffer-quit)
    ;; Use human readable Size column instead of original one
    (define-ibuffer-column size-h
      (:name "Size" :inline t)
      (cond
       ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
       ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
       (t (format "%8d" (buffer-size)))))
    ))

(defun my-misc/init-ibuffer-vc ()
  (use-package ibuffer-vc))

(defun my-misc/init-dictionary ()
  (use-package dictionary
    :defer t
    :init
    (global-set-key [mouse-3] 'dictionary-mouse-popup-matching-words)
    (global-set-key [(control c)(d)] 'dictionary-lookup-definition)
    (global-set-key [(control c)(s)] 'dictionary-search)
    (global-set-key [(control c)(m)] 'dictionary-match-words)
    :config
    ;; choose a dictionary server
    (setq dictionary-server "localhost")))

;; showkey可以显示操作的按及运行的函数
(defun my-misc/init-showkey ()
  (use-package showkey
    :defer t))

(defun my-misc/init-dmenu ()
  (use-package dmenu
    :defer t
    :bind ("s-r" . dmenu)))

(defun my-misc/init-clean-buffers ()
  (use-package clean-buffers
    :config
    (clean-buffers-turn-on-auto-clean-buffers)
    ))

(defun my-misc/init-verify-url ()
  (use-package verify-url
    ))

(defun my-misc/init-start-menu ()
  (use-package start-menu
    :config
    (start-menu-enable)
    (global-set-key (kbd "<mouse-2>") 'start-menu-popup)
    (evil-local-set-key 'normal (kbd "<mouse-2>") 'start-menu-popup)))
(defun my-misc/init-pocket-mode ()
  (use-package pocket-mode
    :defer  t
    :config
    (setq pocket-items-per-page 20)
    (eval-after-load 'pocket-mode
      '(require 'url2org))))
