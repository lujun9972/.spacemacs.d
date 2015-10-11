(require-and-install 'bbdb)
(require 'bbdb-com)
(require 'gnus)
(require 'message)
(require-and-install 'bbdb-gnus)
(require-and-install 'bbdb-vcard)
(require-and-install 'bbdb-android)
(require-and-install 'bbdb-csv-import)
(require-and-install 'bbdb-handy)
(require-and-install 'bbdb-china)

(bbdb-initialize)
;; (bbdb-initialize 'gnus 'message 'mail)
(setq bbdb-mail-user-agent 'gnus-user-agent)

(setq bbdb-file (file-concat MY-GTD-PATH "bbdb"))

(setq bbdb-complete-mail t)
(setq bbdb-completion-list t)
(setq bbdb-complete-mail-allow-cycling t)

(defun format-chinese-phone(args)
  "格式化中国的手机为(+86)13xxxxxxxxx
  格式化中国的电话号码为(区号)xxxxxx"
  (let ((phone-number (remove ?\s (car args)))
        (style (cadr args)))
    (when (= 11 (length phone-number))
      (if (string-prefix-p "1" phone-number)
          (setq phone-number (format "(+86)%s" phone-number))
        (setq phone-number (format "(%s)%s" (substring phone-number 0 4) (substring phone-number 4)))))
    (list phone-number style)))

(advice-add 'bbdb-parse-phone :filter-args #'format-chinese-phone)

(defun bbdb-read-record-advise-function(record)
  "提示存储生日,QQ,微信号"
  (bbdb-record-set-field record 'birthdate
                         (bbdb-read-string "Birthdate (YYYY.MM.DD): "))
  (bbdb-record-set-field record 'QQ
                         (bbdb-read-string "QQ:"))
  (bbdb-record-set-field record 'WeChat
                         (bbdb-read-string "WeChat:"))
  record)

(advice-add 'bbdb-read-record :filter-return #'bbdb-read-record-advise-function)

(global-set-key (kbd "<f9> p") 'bh/phone-call)

;;
;; Phone capture template handling with BBDB lookup
;; Adapted from code by Gregory J. Grubbs
(defun bh/phone-call ()
  "Return name and Organization info for caller from bbdb lookup"
  (interactive)
  (let* (name rec caller)
    (setq name (completing-read "Who is calling? "
                                bbdb-hashtable
                                'bbdb-completion-predicate
                                'confirm))
    (when (> (length name) 0)
      ; Something was supplied - look it up in bbdb
      (setq rec
            (or (first
                 (or (bbdb-search (bbdb-records) name nil nil)
                     (bbdb-search (bbdb-records) nil name nil)))
                name)))

    ; Build the bbdb link if we have a bbdb record, otherwise just return the name
    (setq caller (cond ((and rec (vectorp rec))
                        (let ((name (bbdb-record-name rec))
                              (Organization (bbdb-record-organization rec)))
                          (concat "[[bbdb:"
                                  name "]["
                                  name "]]"
                                  (when Organization
                                    (concat " - " Organization)))))
                       (rec)
                       (t "NameOfCaller")))
    (insert caller)))

(defun eh-bbdb-keybinding ()
  (bbdb-handy-keybinding-setup)
  (define-key bbdb-mode-map "M" 'bbdb-merge-records)
  (define-key bbdb-mode-map (kbd "x e") 'bbdb-android-export)
  (define-key bbdb-mode-map (kbd "x i") 'bbdb-android-import)
  (define-key bbdb-mode-map (kbd "x r") 'bbdb-android-import-from-radicale))

(add-hook 'bbdb-mode-hook 'eh-bbdb-keybinding)
(define-key message-mode-map "\C-cb" 'bbdb-handy)
(define-key message-mode-map "\t" 'bbdb-handy-message-tab)

(provide 'init-GTD-bbdb)
