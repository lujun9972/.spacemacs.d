;;; packages.el --- my-life Layer packages File for Spacemacs
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
(setq my-life-packages
    '(
      ;; package names go here
      emms
      w3m
      ;; org2blog
      sx
      baidu-life
      yahoo-weather
      ;; ego
      ;; org2issue
      ))

;; List of packages to exclude.
(setq my-life-excluded-packages '())

;; For each package, define a function my-life/init-<package-name>
;;
(defun my-life/init-emms ()
  "Initialize my package"
  (use-package emms
    :defer t
    :config
    (require 'emms-setup)
    (emms-all) ;(emms-standard)  
    (emms-default-players)  
    (setq emms-stream-default-action "play") ;当在EMMS stream界面按下RET时的动作
    ;; coding settings  
    (setq emms-info-mp3info-coding-system 'gbk  
          emms-cache-file-coding-system 'utf-8  
          ;; emms-i18n-default-coding-system '(utf-8 . utf-8)  
          )  
    ;; Show the current track each time EMMS  
    ;; starts to play a track with "播放 : "  
    (add-hook 'emms-player-started-hook 'emms-show)  
    (setq emms-show-format "播放: %s")  

    ;; mode line format  
    (setq emms-mode-line-format "[ %s ]"  
          emms-lyrics-display-format "%s"  
          emms-playing-time-display-format "%s")  

    (setq emms-player-list '(emms-player-mplayer emms-player-mpg321) emms-player-mplayer-command-name "mplayer" emms-player-mplayer-parameters '("-slave"))
    (setq emms-repeat-playlist nil
          emms-source-file-default-directory "~/music" ;默认的音乐目录
          emms-lyrics-dir "~/music"
          emms-lyrics-coding-system nil
          emms-playlist-buffer-name "*EMMS*")
    ;; global key-map  
    ;; all global keys prefix is C-c e  
    ;; compatible with emms-playlist mode keybindings  
    ;; you can view emms-playlist-mode.el to get details about  
    ;; emms-playlist mode keys map  
    (global-set-key (kbd "C-c e >") 'emms-seek-forward)  
    (global-set-key (kbd "C-c e <") 'emms-seek-backward)  
    (global-set-key (kbd "C-c e t") 'emms-play-directory-tree)  
    (global-set-key (kbd "C-c e h") 'emms-shuffle)  
    (global-set-key (kbd "C-c e o") 'emms-play-file)  
    (global-set-key (kbd "C-c e l") 'emms-play-playlist)  
    (global-set-key (kbd "C-c e r") 'emms-toggle-repeat-track)  
    (global-set-key (kbd "C-c e R") 'emms-toggle-repeat-playlist)  
    (global-set-key (kbd "C-c e u") 'emms-score-up-playing)  
    (global-set-key (kbd "C-c e d") 'emms-score-down-playing)  
    (global-set-key (kbd "C-c e o") 'emms-score-show-playing)  
    (global-set-key (kbd "C-c e g") 'emms-play-directory)
    (global-set-key (kbd "C-c e d") 'emms-play-dired)
    (global-set-key (kbd "C-c e v") 'emms-playlist-mode-go)
    (global-set-key (kbd "C-c e x") 'emms-start)
    (global-set-key (kbd "C-c e SPC") 'emms-pause)
    (global-set-key (kbd "C-c e s") 'emms-stop)
    (global-set-key (kbd "C-c e n") 'emms-next)
    (global-set-key (kbd "C-c e p") 'emms-previous)
    (global-set-key (kbd "C-c e f") 'emms-show)))

(defun my-life/init-w3m ()
  (use-package w3m
    :config
    (setq w3m-use-favicon nil) 
    ;; 设置w3m主页
    (setq w3m-home-page "http://www.baidu.com")     
    ;; 默认显示图片
    (setq w3m-default-display-inline-images t)
    (setq w3m-default-toggle-inline-images t)
    ;; 使用cookies
    (setq w3m-use-cookies t)
    ;;设定w3m运行的参数，分别为使用cookie和使用框架  
    (setq w3m-command-arguments '("-cookie" "-F"))      
    ;; 使用w3m作为默认浏览器
    (setq browse-url-browser-function 'w3m-browse-url)  
    (setq w3m-view-this-url-new-session-in-background t)
    ;;显示图标
    (setq w3m-show-graphic-icons-in-header-line t)      
    (setq w3m-show-graphic-icons-in-mode-line t) 
    ;;C-c C-p 打开，这个好用               
    (setq w3m-view-this-url-new-session-in-background t)
    (defun remove-w3m-output-garbages ()                
      "去掉w3m输出的垃圾."                                
      (interactive)                                      
      (let ((buffer-read-only))                          
        (setf (point) (point-min))                          
        (while (re-search-forward "[\200-\240]" nil t)      
          (replace-match " "))                                
        (set-buffer-multibyte t))                          
      (set-buffer-modified-p nil))                        
    (add-hook 'w3m-fontify-after-hook
              'remove-w3m-output-garbages)
    ))
(defun my-life/init-org2blog ()
  (use-package org2blog
    :defer t
    :config
    (require 'org2blog-autoloads)

    (setq org2blog/wp-blog-alist
          `(("DarkSun.1kapp.com"
             :url "http://darksun.1kapp.com/xmlrpc.php"
             :username "lujun9972"
             :default-categories ("Emacs")
             :keep-new-lines t
             :confirm t
             :wp-code nil
             :tags-as-categories nil)
            ("DarkSun.blog.51cto.com"
             :url "http://darksun.blog.51cto.com/xmlrpc.php"
             :username "lujun9972"
             :default-categories ("Emacs")
             :keep-new-lines t
             :confirm t
             :wp-code nil
             :tags-as-categories nil)))

    (setq org2blog/wp-buffer-template "#+TITLE: %s\n#+AUTHOR: %s\n#+DATE: %s\n#+OPTIONS: toc:nil num:nil todo:nil pri:nil tags:nil ^:nil\n#+CATEGORY: %s\n#+TAGS:\n#+DESCRIPTION:\n\n")

    (defun org2blog/wp-format-buffer-with-author (buffer-template)
      "Default buffer formatting function."
      (format buffer-template
              ;; TITLE
              (or (plist-get (cdr org2blog/wp-blog) :default-title)
                  org2blog/wp-default-title
                  (read-string "请输入POST标题:"))
              ;; AUTHOR
              user-login-name
              ;; DATE
              (format-time-string "[%Y-%m-%d %a %H:%M]" (current-time))
              ;; CATEGORY
              (mapconcat
               (lambda (cat) cat)
               (or (plist-get (cdr org2blog/wp-blog) :default-categories)
                   org2blog/wp-default-categories)
               ", ")
              ))

    (setq org2blog/wp-buffer-format-function 'org2blog/wp-format-buffer-with-author)))

(defun my-life/init-sx ()
  (use-package sx
    :defer  t))

(defun my-life/init-baidu-life ()
  (use-package baidu-life
    :defer t
    :config
    (setq baidu-life-api-key "fd96cfa5d662e295b9e6d8a32cd8182e")))

(defun my-life/init-yahoo-weather ()
  (use-package sx
    :defer  t))

(defun my-life/init-ego ()
  (use-package ego
    :defer  t))

(defun my-life/init-org2issue ()
  (use-package org2issue
    :defer  t))

;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
