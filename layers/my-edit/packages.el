;;; packages.el --- my-edit Layer packages File for Spacemacs
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
(setq my-edit-packages
    '(
      ;; package names go here
      wgrep
      org-preview-html
      edit-server
      verify-url
      ego
      org2web
      org2issue
      ;; dictionary
      (mdx-dictionary :location (recipe
                                 :fetcher github
                                 :repo "lujun9972/mdx-dictionary.el"
                                 :files ("*")))
      ;; org2blog
      (ispell :location built-in)
      (flyspell :location built-in)
      artbollocks-mode
      langtool
      powerthesaurus
      ))

;; List of packages to exclude.
(setq my-edit-excluded-packages '())

;; For each package, define a function my-edit/init-<package-name>
;;
;; 使用wgrep可以直接修改grep buffer,并将修改映射回原文件
(defun my-edit/init-wgrep ()
  "Initialize my package"
  (use-package wgrep
    :defer t))

(defun my-edit/init-org-preview-html ()
  "Initialize my package"
  (with-eval-after-load 'org (use-package org-preview-html
                               )))

(defun my-edit/init-edit-server ()
  "Initialize my package"
  (use-package edit-server
    :ensure t
    :init (setq edit-server-url-major-mode-alist '(("github\\.com" . gfm-mode)
                                                   ("redmine" . textile-mode))
                edit-server-default-major-mode 'markdown-mode)
    :config (edit-server-start)))

(defun my-misc/init-verify-url ()
  (use-package verify-url
    ))

(defun my-edit/init-ego ()
  (with-eval-after-load 'org
    (use-package ego
      :ensure t
      :config
      (ego-add-to-alist 'ego-project-config-alist
               `(("emacs-document" ; 站点工程的名字
                 :repository-directory "~/github/emacs-document" ; 站点的本地目录
                 :site-domain "https://lujun9972.github.io/emacs-document" ; 站点的网址
                 :site-main-title "EMACS-DOCUMENT" ; 站点的标题
                 :site-sub-title "=============>集思广益" ; 站点的副标题
                 :repository-org-branch "master"
                 :repository-html-branch "gh-pages"
                 :theme (default) ; 使用的主题
                 :summary (("years" :year :updates 10) ("authors" :authors) ("tags" :tags)) ; 导航栏的设置，有 category 和 summary 两种
                 :source-browse-url ("Github" "https://github.com/lujun9972/emacs-document") ; 你的工程源代码所在的位置
                 :personal-disqus-shortname "emacs-document" ; 使用 disqus 评论功能的话，它的短名称
                 ;; :personal-duoshuo-shortname "emacs-document" ; 使用 多说 评论功能的话，它的短名称
                 ;; ;; :confound-email nil ; 是否保护邮件名称呢？t 是保护，nil 是不保护，默认是保护
                 :ignore-file-name-regexp
                 "README.org" ; 有些不想发布成 html 的 org 文件（但是又想被导入 git 进行管理），可以用这种正则表达的方式排除
                 :web-server-docroot "~/webRoot/emacs-document.github.io" ; 本地测试的目录
                 :web-server-port 5432) ; 本地测试的端口
                 ("Emacs公众号文章" ; 站点工程的名字
                 :repository-directory "~/github/emacsist" ; 站点的本地目录
                 :site-domain "https://emacs-china.github.io/emacsist" ; 站点的网址
                 :site-main-title "Emacs公众号文章" ; 站点的标题
                 :site-sub-title "=============>欢迎来稿" ; 站点的副标题
                 :repository-org-branch "master"
                 :repository-html-branch "gh-pages"
                 :theme (default) ; 使用的主题
                 :summary (("years" :year :updates 10) ("authors" :authors) ("tags" :tags)) ; 导航栏的设置，有 category 和 summary 两种
                 :source-browse-url ("Github" "https://github.com/lujun9972/emacs-document") ; 你的工程源代码所在的位置
                 :personal-disqus-shortname "emacsist" ; 使用 disqus 评论功能的话，它的短名称
                 ;; :personal-duoshuo-shortname "emacsist" ; 使用 多说 评论功能的话，它的短名称
                 ;; ;; :confound-email nil ; 是否保护邮件名称呢？t 是保护，nil 是不保护，默认是保护
                 :ignore-file-name-regexp
                 "README.org" ; 有些不想发布成 html 的 org 文件（但是又想被导入 git 进行管理），可以用这种正则表达的方式排除
                 :web-server-docroot "~/webRoot/emacs-china.github.io/emacsist" ; 本地测试的目录
                 :web-server-port 5432)
                 ("lujun9972.github.com" ; 站点工程的名字
                  :repository-directory "~/github/lujun9972.github.com" ; 站点的本地目录
                  :site-domain "https://lujun9972.github.io/" ; 站点的网址
                  :site-main-title "暗无天日" ; 站点的标题
                  :site-sub-title "=============>随便,谢谢" ; 站点的副标题
                  ;; :repository-org-branch "master"
                  ;; :repository-html-branch "gh-pages"
                  :theme (worg) ; 使用的主题
                  :summary (("years" :year :updates 10) ("authors" :authors) ("tags" :tags)) ; 导航栏的设置，有 category 和 summary 两种
                  :source-browse-url ("Github" "https://github.com/lujun9972/lujun9972.github.com") ; 你的工程源代码所在的位置
                  :personal-disqus-shortname "lujun9972" ; 使用 disqus 评论功能的话，它的短名称
                  ;; :personal-duoshuo-shortname "暗日" ; 使用 多说 评论功能的话，它的短名称
                  ;; ;; :confound-email nil ; 是否保护邮件名称呢？t 是保护，nil 是不保护，默认是保护
                  :ignore-file-name-regexp
                  "README.org" ; 有些不想发布成 html 的 org 文件（但是又想被导入 git 进行管理），可以用这种正则表达的方式排除
                  :web-server-docroot "~/webRoot/lujun9972.github.io" ; 本地测试的目录
                  :web-server-port 5432)
                 ;; 你可以在此添加更多的站点设置
                 )))))

(defun my-edit/init-org2web ()
  (with-eval-after-load 'org
    (use-package org2web
      :config
      (org2web-add-project
       '("lujun9972.github.com" ; 站点工程的名字
         :repository-directory "~/github/lujun9972.github.com" ; 站点的本地目录
         :remote (git "https://github.com/lujun9972/lujun9972.github.com.git" "master")
         ;; you can use `rclone` with `:remote (rclone "remote-name" "/remote/path/location")` instead.
         :site-domain "https://lujun9972.github.io/" ; 站点的网址
         :site-main-title "暗无天日"      ; 站点的标题
         :site-sub-title "=============>随便,谢谢" ; 站点的副标题
         :theme (worg)
         :source-browse-url ("Github" "https://github.com/lujun9972/lujun9972.github.com") ; 你的工程源代码所在的位置
         ;; :personal-avatar "/media/img/horse.jpg"
         :personal-disqus-shortname "lujun9972" ; 使用 disqus 评论功能的话，它的短名称
         :web-server-docroot "~/webRoot/lujun9972.github.io" ; 本地测试的目录
         :web-server-port 7654)))))

(defun my-edit/init-org2issue ()
  (with-eval-after-load 'org (use-package org2issue
                               :defer  t)))
(defun my-edit/init-dictionary ()
  (use-package dictionary
    :defer t
    :init
    (global-set-key [mouse-3] 'dictionary-mouse-popup-matching-words)
    (global-set-key [(control c)(d)] 'dictionary-lookup-definition)
    (global-set-key [(control c)(s)] 'dictionary-search)
    (global-set-key [(control c)(m)] 'dictionary-match-words)
    :config
    ;; choose a dictionary server
    (setq dictionary-server "localhost")));;

(defun my-edit/init-mdx-dictionary ()
  (use-package mdx-dictionary
    :defer  t
    :config
    (setq mdx-dictionary-server-args '("-i"))
    (defun mdx-dictionary--save-to-anki (query content)
      (let* ((word (or (and (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))
                       (word-at-point)))
             (sentence (replace-regexp-in-string "[\r\n]+" " " (or (sentence-at-point)
                                                                   (thing-at-point 'line)))) ;去掉句子中的断行
             (sentence (replace-regexp-in-string (regexp-quote word)
                                                 (lambda (word)
                                                   (format "<b>%s</b>" word))
                                                 sentence)) ;高亮句子中的单词
             (content (replace-regexp-in-string "[\r\n]+" "<br>" content))
             (anki-file  "~/mdx-dictionary-for-anki.txt"))
        (with-temp-file anki-file
          (when (file-readable-p anki-file)
            (insert-file-contents anki-file))
          (insert (format "%s|%s|%s\n" query content sentence)))))

    (add-hook 'mdx-dictionary-display-before-functions #'mdx-dictionary--save-to-anki)))

(defun my-edit/init-org2blog ()
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

;; artbollocks-mode.el is an Emacs minor mode for avoiding cliches and bad grammar when writing about art (or other topics).

(defun my-edit/init-artbollocks-mode ()
  "Initialize my package"
  (use-package artbollocks-mode
    :ensure t
    :config
    (add-hook 'text-mode-hook 'artbollocks-mode)
    (setq artbollocks-jargon-regex (concat "\\b" (regexp-opt
                                                  '("u8s"
                                                    "A/D"
                                                    "A/B"
                                                    "ADO") t)
                                           "\\b"))))

(defun my-edit/init-ispell ()
  "Initialize my package"
  (use-package ispell
    :ensure t
    :config
    (setq ispell-dictionary "en")
    (setq ispell-alternate-dictionary "/usr/share/dict/cracklib-small")))

(defun my-edit/init-flyspell ()
  "Initialize my package"
  (use-package flyspell
    :config
    (add-hook 'text-mode-hook 'flyspell-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)))

(defun my-edit/init-langtool ()
  "Initialize my package"
  (use-package langtool
    :ensure t
    :config
    (setq langtool-language-tool-jar "~/Downloads/LanguageTool-4.1/languagetool-commandline.jar")
    (setq langtool-default-language "en-US")
    (setq langtool-mother-tongue "zh-CN")
    (setq langtool-disabled-rules '("WHITESPACE_RULE"
                                    "EN_UNPAIRED_BRACKETS"
                                    "COMMA_PARENTHESIS_WHITESPACE"
                                    "EN_QUOTES"))))

(defun my-edit/init-powerthesaurus ()
  "Initalize powerthesaurus"
  (use-package powerthesaurus
    :ensure t))



;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
