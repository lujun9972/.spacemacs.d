;; 新建文件时,自动创建缺失的目录
(add-to-list 'find-file-not-found-functions #'my-create-non-existent-directory)
;;设置TAB为4个空格
(setq default-tab-width 4)
;;显示光标附近的括号匹配
(show-paren-mode 1)
;; 自动插入引号/括号对
(electric-pair-mode 1)
;; 添加pair项
(add-to-list 'electric-pair-pairs '(?` . ?')) ;添加`'作为匹配项
(add-to-list 'electric-pair-pairs '(?` . ?'))
(add-to-list 'electric-pair-pairs '(?[ . ?]))
(add-to-list 'electric-pair-pairs '(?{ . ?}))

(delete-selection-mode 1)
(global-set-key (kbd "RET") 'newline-and-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUP: Edit -> Back up             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar backup-directory "~/.backups")
(if (eq system-type 'windows-nt)
	(setq backup-directory "d:/backups")
  (setq backup-directory "~/.backups"))

(if (not (file-exists-p backup-directory))
    (make-directory backup-directory t))
(setq
 make-backup-files t        ; backup a file the first time it is saved
 backup-directory-alist `((".*" . ,backup-directory)) ; save backup files in ~/.backups
 backup-by-copying t     ; copy the current file into backup directory
 version-control t   ; version numbers for backup files
 delete-old-versions t   ; delete unnecessary versions
 kept-old-versions 6     ; oldest versions to keep when a new numbered backup is made (default: 2)
 kept-new-versions 9 ; newest versions to keep when a new numbered backup is made (default: 2)
 auto-save-default t ; auto-save every buffer that visits a file
 auto-save-timeout 20 ; number of seconds idle time before auto-save (default: 30)
 auto-save-interval 200 ; number of keystrokes between auto-saves (default: 300)
 )


;; 设置 sentence-end 可以识别中文标点  
(setq sentence-end  
      "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")  
;; 自动判断单个/双个空格表示句子的结束
(defvar sentence-end-double-space-threshold 2
  "How many occurrences of \".  \" per kilobyte should be enough
  to declare this file as using two spaces after sentences.")

(add-hook 'find-file-hook 'set-sentence-end-double-space)	
;; (setq sentence-end-double-space nil)  	;单个空格表示句子的结束

;; 支持emacs和外部程序的粘贴 
(setq x-select-enable-clipboard t)  
