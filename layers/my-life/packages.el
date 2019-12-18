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
      nov
      emms
      w3m
      sx
      podcaster
      youdao-dictionary
      (mdx-dictionary :location (recipe
                                 :fetcher github
                                 :repo "lujun9972/mdx-dictionary.el"
                                 :files ("*")))
      ;; (newsticker :location local)
      (csdn-api :location (recipe
                           :fetcher github
                           :repo "lujun9972/csdn-api.el"))
      anki-connect
      (anki-vocabulary :location (recipe
                              :fetcher github
                              :repo "lujun9972/anki-vocabulary.el"))
      (leetcode :location (recipe
                              :fetcher github
                              :repo "kaiwk/leetcode.el"))
      ))

;; List of packages to exclude.
(setq my-life-excluded-packages '())

;; For each package, define a function my-life/init-<package-name>
;;
(defun my-life/init-nov ()
  "Initialize nov package"
  (use-package nov
    :bind
    (:map nov-mode-map
     ("j" . next-line)
     ("k" . previous-line)
     ("h" . left-char)
     ("l" . right-char)
     ("w" . forward-word)
     ("b" . backward-word))
    :config
    ;; (defun my-nov-font-setup ()
    ;;   (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
    ;;                            :height 1.0))
    ;; (add-hook 'nov-mode-hook 'my-nov-font-setup)
    (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
    (setq nov-save-place-file "~/.emacs.d/.cache/nov-places")
    ))
(defun my-life/init-emms ()
  "Initialize my package"
  (use-package emms
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

    (setq emms-player-list '(emms-player-mpv emms-player-mpg321 emms-player-mplayer)
          emms-player-mplayer-command-name "mplayer"
          emms-player-mplayer-parameters '("-slave")
          emms-player-mpg321-command-name "mpg123")
    (setq emms-repeat-playlist nil
          emms-source-file-default-directory "~/Music" ;默认的音乐目录
          emms-lyrics-dir "~/Music"
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
    (setq w3m-home-page "http://www.baidu.com") ;; 设置w3m主页
    (setq w3m-default-save-directory "~/Download") ;设置默认的保存目录
    (setq w3m-default-display-inline-images t) ;; 默认显示图片
    (setq w3m-default-toggle-inline-images t)
    (setq w3m-toggle-inline-images-permanently t)           ;继续保持当前buffer的图像状态
    (setq w3m-use-cookies t) ;; 使用cookies
    (setq w3m-command-arguments '("-cookie" "-F")) ;;设定w3m运行的参数，分别为使用cookie和使用框架
    ;; (setq browse-url-browser-function 'w3m-browse-url) ;; 使用w3m作为默认浏览器
    (setq w3m-use-header-line-title t)                      ;显示标题
    (setq w3m-cookie-accept-bad-cookies t)                  ;接收 BAD cookie
    (setq w3m-show-graphic-icons-in-header-line t) ;;显示图标
    (setq w3m-show-graphic-icons-in-mode-line t)
    (setq w3m-view-this-url-new-session-in-background t)    ;后台打开连接
    (setq w3m-new-session-in-background t)                  ;后台建立新任务
    (setq w3m-session-time-format "%Y-%m-%d %A %H:%M")      ;上次浏览记录的时间显示格式
    (setq w3m-favicon-use-cache-file t)                     ;使用网站图标的缓存文件
    (setq w3m-keep-arrived-urls 50000)                      ;浏览历史记录的最大值
    (setq w3m-keep-cache-size 1000)                         ;缓存的大小
    (setq w3m-edit-function (quote find-file-other-window)) ;在其他窗口编辑当前页面
    (setq w3m-session-automatic-save t)                     ;退出时自动保存
    (setq w3m-session-deleted-save nil)                     ;关闭一个标签时不保存
    (setq w3m-use-filter t)                                 ;开启过滤
    (setq w3m-fb-mode t)                                    ;让标签和创建它的FRAME关联
    (setq w3m-session-load-crashed-sessions t)              ;默认加载崩溃的对话
    (w3m-fb-mode 1)                                         ;可以显示FRAME
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


(defun my-life/init-sx ()
  (use-package sx
    :defer  t))

(defun my-life/init-newsticker ()
  (use-package newsticker
    :init
    (add-hook 'newsticker-mode-hook 'imenu-add-menubar-index)
    :config
    (add-to-list 'newsticker-url-list '("planet emacs" "http://planet.emacsen.org/atom.xml" nil nil nil)) ;添加planet emacs的RSS
                                        ;(add-to-list 'newsticker-url-list '("陈斌的博客" "http://blog.binchen.org/?feed=rss2" nil nil nil)) ;添加planet emacs的RSS
    (add-to-list 'newsticker-url-list '("Emacs Redux" "http://emacsredux.com/atom.xml" nil nil nil))
    (add-to-list 'newsticker-url-list '("lunaryorn" "http://www.lunaryorn.com/feed.atom" nil nil nil))
    (add-to-list 'newsticker-url-list '("endlessParentheses" "http://endlessparentheses.com/atom.xml" nil nil nil))
    ;; (add-to-list 'newsticker-url-list '("包昊军的博客" "http://baohaojun.github.io/atom.xml" nil nil nil))
    ;; (add-to-list 'newsticker-url-list '("Xudifsd" "http://xudifsd.org/blog/feed/" nil nil nil))
    (add-to-list 'newsticker-url-list '("A programmer's site" "http://shenfeng.me/atom.xml" nil nil nil))
    (add-to-list 'newsticker-url-list '("liutos" "http://liutos.github.io/atom.xml" nil nil nil))
    (add-to-list 'newsticker-url-list '("null programe" "http://nullprogram.com/feed/" nil nil nil))

    ;; (add-to-list 'newsticker-url-list '("Scott Young" "http://feeds.feedburner.com/scotthyoung/HAHx" nil nil nil))
    (setq newsticker-html-renderer 'w3m-region)           ;使用w3m来格式化html
    (setq newsticker-automatically-mark-items-as-old nil) ;不自动将item设置为已读
    (setq newsticker-automatically-mark-visited-items-as-old t) ;自动标记已经访问过的项目
    (setq newsticker-retrieval-interval 600)
    (setq newsticker-retrieval-method 'intern)
    (setq newsticker-show-all-news-elements t)))


(defun my-life/init-csdn-api ()
  (use-package csdn-api
    :defer  t
    ))

(defun my-life/init-podcaster ()
  (use-package podcaster
    :defer  t
    ))

(defun my-life/init-anki-connect ()
  (use-package anki-connect
    :defer  t
    ))

(defun my-life/init-anki-vocabulary ()
  (use-package anki-vocabulary
    :defer  t
    :config
    (setq anki-vocabulary-deck-name "我的生词本")
    (setq anki-vocabulary-model-name "语义本")
    (setq anki-vocabulary-field-alist '(("翻译例句" . "${translation:翻译例句}")
                                        ("读音" . "${phonetic:音标}")
                                        ("意义" . "${glossary:释义}")
                                        ("拼写" . "${expression:单词}")
                                        ("原文例句" . "${sentence_bold:标粗的原文例句}")))
    (setq anki-vocabulary-audio-fileds "读音")
    (when (package-installed-p 'mdx-dictionary)
      (setq anki-vocabulary-word-searcher (lambda (word)
                                            (or (when mdx-dictionary-server-process
                                                  (mdx-dictionary-request word))
                                                (anki-vocabulary--word-searcher-youdao word)))))))

(defun my-life/init-leetcode ()
  (use-package leetcode
    :defer  t
    :config
    (setq leetcode-account "lujun9972")
    (setq leetcode-prefer-language "python3")
    ))

(defun my-life/init-youdao-dictionary ()
  (use-package youdao-dictionary
    :defer  t
    :config
    (setq youdao-dictionary-secret-key "atY68WyfGGoVE5WBc09ihdc2lxZP9sUR")
    (setq youdao-dictionary-app-key "72c03449033eb239")
    ))

(defun my-life/init-mdx-dictionary ()
  (use-package mdx-dictionary
    :defer  t
    :config
    (setq mdx-dictionary-server-args '("-i"))
    (setq mdx-dictionary-parser #'mdx-dictionary--21世纪大英汉词典-parser)
    ))
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

