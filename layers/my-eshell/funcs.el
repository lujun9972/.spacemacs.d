(require 'cl-lib)
;; 定义clear函数
;; (defun eshell/clear ()  
;;   "clear the eshell buffer."  
;;   (interactive)  
;;   (let ((inhibit-read-only t))  
;;     (erase-buffer)))

;; 定义switch函数切换eshell
(defun eshell/switch (&optional buf)
  (let* ((buffers (buffer-list))
         (eshell-buffers (cl-remove-if-not (lambda (buf)
                                             (eq (buffer-local-value 'major-mode buf)
                                                 'eshell-mode)) buffers))
         (eshell-buffer-names (mapcar #'buffer-name eshell-buffers))
         (buf (or buf (completing-read "switch to: " eshell-buffer-names))))
    (if (member buf eshell-buffer-names)
        (switch-to-buffer buf)
      (eshell t)
      (rename-buffer buf))))

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



;; 与bookmark的整合
(require 'bookmark)
(require 'cl-lib)

(defun eshell/marks ()
  (bookmark-maybe-load-default-file)
  (let* ((bookmarks (bookmark-maybe-sort-alist))
         (directory-bookmarks (cl-remove-if-not (lambda (bookmark)
                                               (file-directory-p (bookmark-get-filename bookmark)))
                                             bookmarks))
         (show-bookmark-fn (lambda (bookmark)
                             (let ((name (bookmark-name-from-full-record bookmark))
                                   (filename (bookmark-get-filename bookmark)))
                               (format "%s	->	%s" name filename)))))
    (mapconcat show-bookmark-fn directory-bookmarks "\n")))

(defun eshell/jump (bookmark-name)
  (bookmark-maybe-load-default-file)
  (let ((filename (bookmark-get-filename bookmark-name)))
    (if (file-directory-p filename)
        (eshell/cd filename)
      (error "%s is not a directory" bookmark-name))))

(defun pcmpl-bookmark-names (&optional name)
  "Return a list of directory bookmark names"
  (bookmark-maybe-load-default-file)
  (let* ((name (or name ""))
         (bookmarks (bookmark-maybe-sort-alist))
         (directory-bookmarks (cl-remove-if-not (lambda (bookmark)
                                               (file-directory-p (bookmark-get-filename bookmark)))
                                             bookmarks))
         (bookmark-names (mapcar #'bookmark-name-from-full-record directory-bookmarks)))
    (cl-remove-if-not (lambda (bookmark-name)
                     (string-prefix-p name bookmark-name))
                   bookmark-names)))

(defun pcomplete/jump ()
  "completion for `jump'"
  (while
      (pcomplete-here
       (pcmpl-bookmark-names (pcomplete-arg 'last)))))

(defun eshell/mark (&optional bookmark-name no-overwrite)
  (let ((buffer-file-name default-directory))
    (bookmark-set bookmark-name no-overwrite)))

(defalias 'eshell/unmark 'bookmark-delete)
