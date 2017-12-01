;; eww
(setq eww-download-directory "d:/Download") ;下载目录
(setq eww-search-prefix "http://www.baidu.com/baidu?wd=") ;使用baidu进行搜索
(require 'eww-auto-reload)								  ;当eww访问的url是file url时,提供类似auto-revert的功能.

;;设置newsticker,阅读RSS


;; goto-address-mode允许你在buffer中高亮并点击URL
(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(add-hook 'text-mode-hook 'goto-address-mode)
;;默认链接网络浏览器打开  
(setq browse-url-generic-program (executable-find "firefox")  
      ;; browse-url-browser-function 'w3m-browse-url
      )  
