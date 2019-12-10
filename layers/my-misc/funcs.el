(defun set-info-keys()
  ""
  (interactive)
  (local-set-key (kbd "<mouse-4>") 'mwheel-scroll )
  (local-set-key (kbd "<mouse-5>") 'mwheel-scroll ) )

;; 使用C-h C-f查看函数的源代码
(defun find-function-view-mode (fun)
  (interactive (find-function-read))
  (find-function-do-it fun nil 'switch-to-buffer)
  (view-mode 1))


;; 用org-mode保存firefox session
(defun save-firefox-session ()
  "Reads chromium current session and converts it to org-mode chunk."
  (interactive)
  (save-excursion
    (let* ((path "~/.mozilla/firefox/*.default/sessionstore-backups/recovery.jsonlz4")
           (cmd (concat "lz4jsoncat " path " | grep -oP '\"(http.+?)\"' | sed 's/\"//g' | sort | uniq"))
           (ret (shell-command-to-string cmd)))
      (insert
       (concat
        "* "
        (format-time-string "[%Y-%m-%d %H:%M:%S]")
        "\n"
        (mapconcat 'identity
                   (cl-reduce (lambda (lst x)
                                (if (and x (not (string= "" x)))
                                    (cons (concat " - " x) lst)
                                  lst))
                              (split-string ret "\n")
                              :initial-value (list))
                   "\n"))))))

(defun restore-firefox-session ()
  "Restore session, by openning each link in list with (browse-url).
  Make sure to put cursor on date heading that contains list of urls."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\*")
      (forward-line 1)
      (while (looking-at "^[ ]+-[ ]+\\(http.?+\\)$")
        (let* ((ln (thing-at-point 'line t))
               (ln (replace-regexp-in-string "^[ ]+-[ ]+" "" ln))
               (ln (replace-regexp-in-string "\n" "" ln)))
          (browse-url ln))
        (forward-line 1)))))
