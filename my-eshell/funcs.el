;; 定义clear函数
(defun eshell/clear ()  
  "clear the eshell buffer."  
  (interactive)  
  (let ((inhibit-read-only t))  
    (erase-buffer)))  

;; 定义find-files一次打开多个文件
(defun eshell/find-files (&rest args)
  (when (not (null args))
    "使用find-file打开多个文件"
    (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

;; 定义eshell/j函数用于选择跳转到之前的哪个目录
(defun eshell/j ()
  "Quickly jump to previous directories."
  (eshell/cd (ido-completing-read "Jump to directory: "
                                  (delete-dups (ring-elements eshell-last-dir-ring)))))
;; 定义eshell/r使用ido方式选择eshell历史
(defun eshell/r ()
  "use ido-style to read  eshell-history"
  (interactive)
  (let* ((index 0)
         (peng-ido-eshell-list nil)
         (end (point))
         (beg (save-excursion (eshell-bol) (point)))
         (input (buffer-substring beg end)))
    (while (<= index eshell-history-size)
      (add-to-list 'peng-ido-eshell-list (eshell-get-history index))
      (setq index (1+ index)))
    (setq peng-ido-eshell-list (delete-dups (reverse peng-ido-eshell-list)))
    (if (equal input "")
        (insert (ido-completing-read "Eshell-history: " peng-ido-eshell-list))
      (goto-char beg)
      (kill-line)
      (insert (ido-completing-read "Eshell-history: " peng-ido-eshell-list
                                   nil
                                   nil
                                   input)))))

;; 定义eshell/c使用ido方式选择cd某个bookmark中的目录
(defun eshell/c ()
  "Quickly jump to bookmark directories."
  (require 'bookmark)
  (let* ((directory-bookmarks (remove-if-not (lambda (bookmark)
                                               (file-directory-p (bookmark-get-filename bookmark))) bookmark-alist))
         (directory-bookmark-names (mapcar #'car directory-bookmarks))
         (directory-bookmark-name (ido-completing-read "Jump to directory: "
                                                       (delete-dups directory-bookmark-names)))
         (directory-bookmark-directory (bookmark-get-filename directory-bookmark-name)))
    (eshell/cd directory-bookmark-directory)))

;; 定义eshell/open使用操作系统默认设置打开文件
(defun eshell/open (&rest args)
  "使用操作系统默认设置打开文件"
  (cond ((eq system-type 'windows-nt)
         (mapc (lambda (file)
                 "win32环境下打开file"
                 (w32-shell-execute "open" file)) (eshell-flatten-list (reverse args))))
        ((eq system-type 'gnu/linux)
         (mapc (lambda (file)
                 "linux环境下打开file"
                 (let ((process-connection-type nil))
                   (start-process "" nil "xdg-open" file))) (eshell-flatten-list (reverse args))))
        (t (error "暂不支持该类型的操作系统%s" system-type))))
