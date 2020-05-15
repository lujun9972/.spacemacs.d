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
      (ego :location (recipe
                      :fetcher github
                      :repo "lujun9972/EGO"
                      :files ("*")))
      org2issue
      ;; dictionary
      (smms :location (recipe
                                 :fetcher github
                                 :repo "lujun9972/smms.el"
                                 :files ("*.el")))
      org2blog
      (ispell :location built-in)
      (flyspell :location built-in)
      (delim-col :location built-in)
      artbollocks-mode
      langtool
      powerthesaurus
      pyim
      ;; define-word
      org-roam
      org-special-block-extras
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

(defun my-edit/init-verify-url ()
  (use-package verify-url
    :ensure t
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
                 :store-dir "~/webRoot/emacs-document.github.io" ; 本地测试的目录
                 ) ; 本地测试的端口
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
                 :store-dir "~/webRoot/emacs-china.github.io/emacsist" ; 本地测试的目录
                 )
                 ("lujun9972.github.com" ; 站点工程的名字
                  :repository-directory "~/github/lujun9972.github.com" ; 站点的本地目录
                  :site-domain "https://www.lujun9972.win/" ; 站点的网址
                  :site-main-title "暗无天日"                 ; 站点的标题
                  :site-sub-title "=============>随便,谢谢"   ; 站点的副标题
                  ;; :repository-org-branch "master"
                  ;; :repository-html-branch "gh-pages"
                  :theme (DarkSun)         ; 使用的主题
                  :summary (("years" :year :updates 10) ("authors" :authors) ("tags" :tags)) ; 导航栏的设置，有 category 和 summary 两种
                  :source-browse-url ("Github" "https://github.com/lujun9972/lujun9972.github.com") ; 你的工程源代码所在的位置
                  :personal-disqus-shortname "lujun9972" ; 使用 disqus 评论功能的话，它的短名称
                  :personal-google-analytics-id "7bac4fd0247f69c27887e0d4e3aee41e"
                  ;; :personal-duoshuo-shortname "暗日" ; 使用 多说 评论功能的话，它的短名称
                  ;; ;; :confound-email nil ; 是否保护邮件名称呢？t 是保护，nil 是不保护，默认是保护
                  :ignore-file-name-regexp
                  "README.org" ; 有些不想发布成 html 的 org 文件（但是又想被导入 git 进行管理），可以用这种正则表达的方式排除
                  :store-dir "~/webRoot/lujun9972.github.io" ; 本地测试的目录
                  )
                 ;; 你可以在此添加更多的站点设置
                 )))))

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


(defun my-edit/init-org2blog ()
  (use-package org2blog
    :defer t
    :config
    (require 'org2blog-autoloads)

    (setq org2blog/wp-blog-alist
          `(("baby.lujun9972.win"
             :url "https://baby.lujun9972.win/baby/xmlrpc.php"
             :username "lujun9972"
             :default-categories ("Baby")
             :keep-new-lines t
             :confirm t
             :wp-code nil
             :tags-as-categories t)
            ("DarkSun.blog.51cto.com"
             :url "https://darksun.blog.51cto.com/xmlrpc.php"
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
                  ;; org2blog/wp-default-title
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

    (setq org2blog/wp-buffer-format-function 'org2blog/wp-format-buffer-with-author)
    (setq org2blog/wp-show-post-in-browser 'ask)))

;; artbollocks-mode.el is an Emacs minor mode for avoiding cliches and bad grammar when writing about art (or other topics).

(defun my-edit/init-artbollocks-mode ()
  "Initialize my package"
  (use-package artbollocks-mode
    :ensure t
    :config
    ;; (add-hook 'text-mode-hook 'artbollocks-mode)
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

(defun my-edit/init-delim-col ()
  "Initialize my package"
  (use-package delim-col
    :config
    (defun my-delimits-column-region (orig-fun &rest args)
      (let
          ((delimit-columns-separator
            (read-regexp
             (format "%s (%s): " "Specify the regexp which separates each column" delimit-columns-separator)
             (list delimit-columns-separator)))
           (delimit-columns-before
            (read-string
             (format "%s (%s): " "Specify a string to be inserted before each column" delimit-columns-before)
             nil nil delimit-columns-before))
           (delimit-columns-after
            (read-string
             (format "%s (%s): " "Specify a string to be inserted after each column" delimit-columns-after)
             nil nil delimit-columns-after))
           (delimit-columns-str-separator
            (read-string
             (format "%s (%s): " "Specify a string to be inserted between each column" delimit-columns-str-separator)
             nil nil delimit-columns-str-separator))
           (delimit-columns-str-before
            (read-string
             (format "%s (%s): " "Specify a string to be inserted before the first column" delimit-columns-str-before)
             nil nil delimit-columns-str-before))
           (delimit-columns-str-after
            (read-string
             (format "%s (%s): " "Specify a string to be inserted after the last column" delimit-columns-str-after)
             nil nil delimit-columns-str-after))
           (delimit-columns-format
            (let*
                ((choices
                  '(("Align Columns" . t)
                    ("No Formatting")
                    ("Align Separators" . separator)
                    ("Pad Columns" . padding)))
                 (default-choice
                   (car
                    (rassoc delimit-columns-format choices)))
                 (choice
                  (completing-read
                   (format "%s (%s): " "Specify how to format columns" default-choice)
                   choices nil t nil nil default-choice)))
              (message "%s" choice)
              (assoc-default choice choices))))
        (apply orig-fun args)))

    (advice-add 'delimit-columns-region :around #'my-delimits-column-region)
    (advice-add 'delimit-columns-rectangle :around #'my-delimits-column-region)

    (define-key-after global-map
      [menu-bar extra-tools]
      (cons "Extra Tools"
            (easy-menu-create-menu "Extra Tools" nil))
      'tools)

    (easy-menu-define my-delim-col-menu nil "Menu for Delim Col"
      '("Delimit Columns in ..."
        ["Region" delimit-columns-region :help "Prettify all columns in a text region"]
        ["Rectangle" delimit-columns-rectangle :help "Prettify all columns in a text rectangle"]
        "---"
        ["Customize" delimit-columns-customize :help "Customization of `columns' group"]))

    (easy-menu-add-item (current-global-map) '("menu-bar" "extra-tools") my-delim-col-menu)))

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

(defun my-edit/init-smms ()
  "Initalize smms"
  (use-package smms
    :ensure t))

(defun my-edit/init-pyim ()
  "Initalize pyim"
  (use-package pyim
    :ensure t
    :demand t
    :config
    ;; 激活 basedict 拼音词库，五笔用户请继续阅读 README
    (use-package pyim-basedict
      :ensure t
      :config (pyim-basedict-enable))

    (setq default-input-method "pyim")

    ;; 我使用全拼
    (setq pyim-default-scheme 'quanpin)

    ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
    ;; 我自己使用的中英文动态切换规则是：
    ;; 1. 光标只有在注释里面时，才可以输入中文。
    ;; 2. 光标前是汉字字符时，才能输入中文。
    ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
    (setq-default pyim-english-input-switch-functions
                  '(pyim-probe-dynamic-english
                    pyim-probe-isearch-mode
                    pyim-probe-program-mode
                    pyim-probe-org-structure-template))

    (setq-default pyim-punctuation-half-width-functions
                  '(pyim-probe-punctuation-line-beginning
                    pyim-probe-punctuation-after-punctuation))

    ;; 开启拼音搜索功能
    (pyim-isearch-mode 1)

    ;; 使用 pupup-el 来绘制选词框, 如果用 emacs26, 建议设置
    ;; 为 'posframe, 速度很快并且菜单不会变形，不过需要用户
    ;; 手动安装 posframe 包。
    (use-package posframe
      :ensure t)
    (setq pyim-page-tooltip 'posframe)

    ;; 选词框显示9个候选词
    (setq pyim-page-length 9)

    ;; 使用liberime
    (when (file-exists-p "~/.emacs.d/liberime.so")
      (setq load-path (cons (file-truename "~/.emacs.d/") load-path))
      (require 'liberime)
      (liberime-start "/usr/share/rime-data/" (file-truename "~/.emacs.d/pyim/rime/"))
      (liberime-select-schema "luna_pinyin_simp")
      (setq pyim-default-scheme 'rime-quanpin))
    :bind
    (("M-J" . pyim-convert-string-at-point) ;与 pyim-probe-dynamic-english 配合
     ("C-;" . pyim-delete-word-from-personal-buffer))))

(defun my-edit/init-org-roam ()
  (use-package org-roam
    :hook
    (after-init . org-roam-mode)
    :custom
    (org-roam-directory MY-NOTE-PATH)
    :init
    (progn
      (spacemacs/declare-prefix "ar" "org-roam")
      (spacemacs/set-leader-keys
        "arl" 'org-roam
        "art" 'org-roam-today
        "arf" 'org-roam-find-file
        "arg" 'org-roam-show-graph)

      (spacemacs/declare-prefix-for-mode 'org-mode "mr" "org-roam")
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "rl" 'org-roam
        "rt" 'org-roam-today
        "rb" 'org-roam-switch-to-buffer
        "rf" 'org-roam-find-file
        "ri" 'org-roam-insert
        "rg" 'org-roam-show-graph))))

;; (defun my-edit/init-define-word ()
;;   "Initalize powerthesaurus"
;;   (use-package define-word
;;     :ensure t))

(defun my-GTD/init-org-specail-block-extras ()
  (use-package org-special-block-extras
    :ensure t
    :hook (org-mode . org-special-block-extras-mode)))

;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
