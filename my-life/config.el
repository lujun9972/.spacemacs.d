;; eww
(setq eww-download-directory "d:/Download") ;下载目录
(setq eww-search-prefix "http://www.baidu.com/baidu?wd=") ;使用baidu进行搜索
(require 'eww-auto-reload)								  ;当eww访问的url是file url时,提供类似auto-revert的功能.

;;设置newsticker,阅读RSS
(use-package newsticker
  :init
  (add-hook 'newsticker-mode-hook 'imenu-add-menubar-index) 
  :defer t
  :config
  (add-to-list 'newsticker-url-list '("planet emacs" "http://planet.emacsen.org/atom.xml" nil nil nil)) ;添加planet emacs的RSS
                                        ;(add-to-list 'newsticker-url-list '("陈斌的博客" "http://blog.binchen.org/?feed=rss2" nil nil nil)) ;添加planet emacs的RSS
  (add-to-list 'newsticker-url-list '("Emacs Redux" "http://emacsredux.com/atom.xml" nil nil nil))
  (add-to-list 'newsticker-url-list '("lunaryorn" "http://www.lunaryorn.com/feed.atom" nil nil nil))
  (add-to-list 'newsticker-url-list '("endlessParentheses" "http://endlessparentheses.com/atom.xml" nil nil nil))
  (add-to-list 'newsticker-url-list '("包昊军的博客" "http://baohaojun.github.io/atom.xml" nil nil nil))
  ;; (add-to-list 'newsticker-url-list '("Xudifsd" "http://xudifsd.org/blog/feed/" nil nil nil))
  (add-to-list 'newsticker-url-list '("A programmer's site" "http://shenfeng.me/atom.xml" nil nil nil))
  (add-to-list 'newsticker-url-list '("null programe" "http://nullprogram.com/feed/" nil nil nil))

  ;; (add-to-list 'newsticker-url-list '("Scott Young" "http://feeds.feedburner.com/scotthyoung/HAHx" nil nil nil)) 
  (setq newsticker-html-renderer 'w3m-region)           ;使用w3m来格式化html
  (setq newsticker-automatically-mark-items-as-old nil) ;不自动将item设置为已读
  (setq newsticker-show-all-news-elements t))

;; goto-address-mode允许你在buffer中高亮并点击URL
(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(add-hook 'text-mode-hook 'goto-address-mode)
;;默认链接网络浏览器打开  
(setq browse-url-generic-program (executable-find "firefox")  
      browse-url-browser-function 'w3m-browse-url)  
