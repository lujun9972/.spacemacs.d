; Enable habit tracking (and a bunch of other modules)
(mapc (lambda (pkg)
        (require pkg nil t))
      '(org-agenda
        org-bbdb
        org-bibtex
        org-crypt
        org-gnus
        org-id
        org-info
        org-jsinfo
        org-habit
        org-inlinetask
        org-irc
        org-mew
        org-mhe
        org-protocol
        org-rmail
        org-vm
        org-wl
        org-w3m
        org-eww
        ox-md
        ob-shell))

(let ((keys (quote (("r" "org-remember" "") ("l" "org-store-link" "") ("c" "org-capture" "") ("a" "org-agenda" "") ("b" "org-iswitchb" "") ("<f11>" "org-clock-goto" "f11:跳转到正在计时的任务") ("" "" "")))))
(mapc (lambda (key)
        (let ((k (car key))
              (f (intern (cadr key))))
          (when (functionp f)
            (global-set-key (kbd k) f))))
      keys)
)

(setq org-agenda-files (list (concat MY-GTD-PATH "home.org")
                             (concat MY-GTD-PATH "office.org")
                             (concat MY-GTD-PATH "Note.org")
                             (concat MY-GTD-PATH "regular.org")
                             (concat MY-GTD-PATH "sms-bank.org")))
(setq org-agenda-include-diary t)       ;将diary的事项也纳入agenda中显示

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      '(("n" "Notes" tags "NOTE"
         ((org-agenda-overriding-header "Notes")
          (org-tags-match-list-sublevels t)))
        ("m" "Home" tags "HOME"
         ((org-agenda-overriding-header "Home")
          (org-tags-match-list-sublevels t)))
        ("o" "Office" tags "OFFICE"
         ((org-agenda-overriding-header "Office")
          (org-tags-match-list-sublevels t)))
        ("h" "Habits" tags-todo "STYLE=\"habit\""
         ((org-agenda-overriding-header "Habits")
          (org-agenda-sorting-strategy
           '(todo-state-down effort-up category-keep))))
        (" " "Agenda"
         ((agenda "" nil)
          (tags "REFILE"
                ((org-agenda-overriding-header "Tasks to Refile")
                 (org-tags-match-list-sublevels nil)))
          ;; (tags-todo "-CANCELLED/!"
          ;;             ((org-agenda-overriding-header "Stuck Projects")
          ;;              (org-agenda-skip-function 'bh/skip-non-stuck-projects)
          ;;              (org-agenda-sorting-strategy
          ;;               '(category-keep))))
          ;; (tags-todo "-HOLD-CANCELLED/!"
          ;;             ((org-agenda-overriding-header "Projects")
          ;;              (org-agenda-skip-function 'bh/skip-non-projects)
          ;;              (org-tags-match-list-sublevels 'indented)
          ;;              (org-agenda-sorting-strategy
          ;;               '(category-keep))))
          ;; (tags-todo "-CANCELLED/!NEXT"
          ;;             ((org-agenda-overriding-header (concat "Project Next Tasks"
          ;;                                                    (if bh/hide-scheduled-and-waiting-next-tasks
          ;;                                                        ""
          ;;                                                      " (including WAITING and SCHEDULED tasks)")))
          ;;              (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
          ;;              (org-tags-match-list-sublevels t)
          ;;              (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
          ;;              (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
          ;;              (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
          ;;              (org-agenda-sorting-strategy
          ;;               '(todo-state-down effort-up category-keep))))
          ;; (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
          ;;             ((org-agenda-overriding-header (concat "Project Subtasks"
          ;;                                                    (if bh/hide-scheduled-and-waiting-next-tasks
          ;;                                                        ""
          ;;                                                      " (including WAITING and SCHEDULED tasks)")))
          ;;              (org-agenda-skip-function 'bh/skip-non-project-tasks)
          ;;              (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
          ;;              (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
          ;;              (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
          ;;              (org-agenda-sorting-strategy
          ;;               '(category-keep))))
          ;; (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
          ;;             ((org-agenda-overriding-header (concat "Standalone Tasks"
          ;;                                                    (if bh/hide-scheduled-and-waiting-next-tasks
          ;;                                                        ""
          ;;                                                      " (including WAITING and SCHEDULED tasks)")))
          ;;              (org-agenda-skip-function 'bh/skip-project-tasks)
          ;;              (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
          ;;              (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
          ;;              (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
          ;;              (org-agenda-sorting-strategy
          ;;               '(category-keep))))
          ;; (tags-todo "-CANCELLED+WAITING|HOLD/!"
          ;;             ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
          ;;                                                    (if bh/hide-scheduled-and-waiting-next-tasks
          ;;                                                        ""
          ;;                                                      " (including WAITING and SCHEDULED tasks)")))
          ;;              (org-agenda-skip-function 'bh/skip-non-tasks)
          ;;              (org-tags-match-list-sublevels nil)
          ;;              (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
          ;;              (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
          ;; (tags "-REFILE/"
          ;;        ((org-agenda-overriding-header "Tasks to Archive")
          ;;         (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
          ;;         (org-tags-match-list-sublevels nil)))
          (alltodo ""))
         nil)))

;; 当搜索文本时,也从归档文件中查找
(setq org-agenda-text-search-extra-files (quote (agenda-archives)))

;; Show all future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all t)

;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)

;; Sorting order for tasks on the agenda
;; (setq org-agenda-sorting-strategy
;;       '((agenda habit-down time-up user-defined-up effort-up category-keep)
;;      (todo category-up effort-up)
;;      (tags category-up effort-up)
;;      (search category-up)))

;; Start the weekly agenda on Monday
(setq org-agenda-start-on-weekday 1)

;; Enable display of the time grid so we can see the marker for the current time
;; (setq org-agenda-time-grid (quote ((daily today remove-match)
;;                                    #("----------------" 0 16 (org-heading t))
;;                                    (0900 1100 1300 1500 1700))))

;; Display tags farther right
(setq org-agenda-tags-column -102)

;; Use sticky agenda's so they persist
(setq org-agenda-sticky t)

(setq org-deadline-warning-days 10)

(defun bh/auto-exclude-function (tag)
  "Automatic task exclusion in the agenda with / RET"
  (and (cond
        ((string= tag "hold")
         t))
       (concat "-" tag)))

(setq org-agenda-auto-exclude-function 'bh/auto-exclude-function)

;; Keep tasks with dates on the global todo lists
(setq org-agenda-todo-ignore-with-date nil)

;; Keep tasks with deadlines on the global todo lists
(setq org-agenda-todo-ignore-deadlines nil)

;; Keep tasks with scheduled dates on the global todo lists
(setq org-agenda-todo-ignore-scheduled nil)

;; Keep tasks with timestamps on the global todo lists
(setq org-agenda-todo-ignore-timestamp nil)

;; Remove completed deadline tasks from the agenda view
(setq org-agenda-skip-deadline-if-done t)

;; Remove completed scheduled tasks from the agenda view
(setq org-agenda-skip-scheduled-if-done t)

;; Remove completed items from search results
(setq org-agenda-skip-timestamp-if-done t)

;; (setq org-agenda-persistent-filter t)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

(setq org-use-fast-todo-selection t)

(setq org-treat-S-cursor-todo-selection-as-state-change nil)

;; 任何未完成的子任务会阻止父任务变为完成状态,若像临时屏蔽该功能,可以为该任务添加`:NOBLOCKING: t'属性
;; 若父任务中设置了属性`:ORDERED: t',则表示其子任务必须依照顺序从上到下完成
(setq org-enforce-todo-dependencies t)

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

;; * Moving a task to CANCELLED adds a CANCELLED tag
;; * Moving a task to WAITING adds a WAITING tag
;; * Moving a task to HOLD adds WAITING and HOLD tags
;; * Moving a task to a done state removes WAITING and HOLD tags
;; * Moving a task to TODO removes WAITING, CANCELLED, and HOLD tags
;; * Moving a task to NEXT removes WAITING, CANCELLED, and HOLD tags
;; * Moving a task to DONE removes WAITING, CANCELLED, and HOLD tags

(defun bh/mark-next-parent-tasks-todo ()
  "Visit each parent task and change NEXT states to TODO"
  (let ((mystate (nth 2 (org-heading-components))))
    (when mystate
      (save-excursion
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) (list "NEXT"))
            (org-todo "TODO")))))))

(add-hook 'org-after-todo-state-change-hook 'bh/mark-next-parent-tasks-todo 'append)
(add-hook 'org-clock-in-hook 'bh/mark-next-parent-tasks-todo 'append)

;; 改为使用org-ident-mode隐藏
;; (setq org-hide-leading-stars nil)

(setq org-startup-indented t)

(setq org-cycle-separator-lines 0)

;; 将note放在task的首部显示
(setq org-reverse-note-order nil)

(setq org-log-done (quote time))

(setq org-log-into-drawer t)

(setq org-log-state-notes-insert-after-drawers nil)

(require 'org-clock)
(setq org-clock-sound t)

;; 由于一般使用org来做笔记,因此默认不自动添加inactive timestamp
(defvar bh/insert-inactive-timestamp nil)

(defun bh/toggle-insert-inactive-timestamp ()
  (interactive)
  (setq bh/insert-inactive-timestamp (not bh/insert-inactive-timestamp))
  (message "Heading timestamps are %s" (if bh/insert-inactive-timestamp "ON" "OFF")))

;; <f9> t 用来切换是否自动添加inactive timestamp
(global-set-key (kbd "<f9> t") 'bh/toggle-insert-inactive-timestamp)

(defun bh/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun bh/insert-heading-inactive-timestamp ()
  (save-excursion
    (when bh/insert-inactive-timestamp
      (org-return)
      (org-cycle)
      (bh/insert-inactive-timestamp))))

(add-hook 'org-insert-heading-hook 'bh/insert-heading-inactive-timestamp 'append)

(setq org-export-with-timestamps nil)

(setq org-clone-delete-id t)

(setq org-cycle-include-plain-lists t)

(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

(require 'org-capture)

(setq org-default-notes-file (concat MY-GTD-PATH "refile.org"))
(setq org-capture-templates
      '(("t" "TODO" entry (file (concat MY-GTD-PATH "refile.org" ))
         "* TODO %? \n%U\n%a\n" :clock-in t :clock-resume t) 
        ("r" "respond" entry (file (concat MY-GTD-PATH "refile.org" ))
         "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
        ("R" "Regular" entry (file+datetree (concat MY-GTD-PATH "regular.org" ))
         "* %?" :clock-in t :clock-resume t)
        ("n" "Note" entry (file+headline (concat MY-GTD-PATH "Note.org" ) "Notes")
         "* %? %x %^g" :clock-in t :clock-resume t)
        ("d" "diary" entry (file+datetree (concat MY-GTD-PATH "diary.org" ))
         "* %?\n" :clock-in t :clock-resume t)
        ("j" "interuption" entry (file+datetree (concat MY-GTD-PATH "refile.org" ))
         "* %?\n" :clock-in t :clock-resume t)
        ("w" "org-protocol" entry (file (concat MY-GTD-PATH "refile.org" ))
         "* TODO Review %c\n%U\n" :immediate-finish t)
        ("m" "Meeting" entry (file (concat MY-GTD-PATH "refile.org"))
         "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
        ("p" "Phone call" entry (file (concat MY-GTD-PATH "refile.org"))
         "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
        ("h" "Habit" entry (file (concat MY-GTD-PATH "refile.org"))
         "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
        ("i" "Idea" entry (file (concat MY-GTD-PATH "refile.org" ))
         "* %? %x %a"  :clock-in t :clock-resume t) 
        ("b" "Books" entry (file (concat MY-GTD-PATH "books.org" ))
         "** TODO %^{书籍名称？}  :book:"  :clock-in t :clock-resume t)))

;; Remove empty LOGBOOK drawers on clock out
;; (defun bh/remove-empty-drawer-on-clock-out ()
;;   (interactive)
;;   (save-excursion
;;     (beginning-of-line 0)
;;     (org-remove-empty-drawer-at (point))))

;; (add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

;; 可以refile到`org-agenda-files'中的文件和当前文件中. 最多9层深度
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

;; Use full outline paths for refile targets - we file directly with IDO
;; 这时,可以使用/level1/level2/level3来表示一个三层的headline
(setq org-refile-use-outline-path t)

;; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
;; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

;;;; Refile settings
;; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

(require 'org-clock)
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;;
;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; 当clock in某个task,则自动更改该task状态为NEXT,不会对capture task,project和sub project有效果
;; 当clock in某个project/sub project,则自动更改NEXT状态为TODO
(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
;; Separate drawers for clocking and logs
;; (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution  'when-no-clock-is-running)
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(defvar bh/keep-clock-running nil)

(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (bh/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (bh/is-project-p))
      "TODO"))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
                                        ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))

(defvar bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
    (org-clock-in '(16)))) ; clock into the current task and mark it as the default task, a special task that will always be offered in the clocking selection, associated with the letter `d'.


(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))

;; 若一个task被clock out,则父级project被自动clock in. 若没有父级project则自动clock in default task
(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

(require 'org-id)
(defun bh/clock-in-task-by-id (id)
  "Clock in a task by id"
  (org-with-point-at (org-id-find id 'marker)
    (org-clock-in nil)))

(defun bh/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (widen)
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))

(setq org-time-stamp-rounding-minutes '(1 1))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button)))) t))

;; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist
      '(:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80))

;; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %TODO %10Effort(Effort){:} %10CLOCKSUM %TAGS")

(setq org-agenda-log-mode-items  '(closed state clock))

(setq org-tag-persistent-alist '((:startgroup)
                      ("@office" . ?o)
                      ("@home" . ?h)
                      ("@traffice" . ?t)
                      ("@market" . ?m)
                      (:endgroup)
                      ("HOME" . ?H)
                      ("OFFICE" . ?O)
                      ("WAITING" . ?w)
                      ("HOLD" . ?h)
                      ("NOTE" . ?n)
                      ("CANCELLED" . ?c)))

; Allow setting single tags without the menu
;; (setq org-fast-tag-selection-single-key (quote expert))

; For tag searches ignore tasks with scheduled and deadline dates
;; (setq org-agenda-tags-todo-honor-ignore-options t)

;; 所有有子任务的task都被认为是project
;; 若project的子树中有"NEXT"状态task的,不认为是stucked
(setq org-stuck-projects '("+LEVEL=2/-DONE" ("NEXT") nil ""))

(setq org-id-method 'org)               ;使用org内置的方法生成

(require 'org-archive)

(setq org-archive-mark-done nil)

;; (setq org-archive-location "%s_archive::* Archived Tasks")

;; 允许使用单字母bullets
(setq org-list-allow-alphabetical t)

;; Explicitly load required exporters
(require 'ox-html)
;; (require 'ox-latex)
(require 'ox-ascii)

(setq org-table-export-default-format "orgtbl-to-csv")

;; experimenting with docbook exports - not finished
;; (setq org-export-docbook-xsl-fo-proc-command "fop %s %s")
;; (setq org-export-docbook-xslt-proc-command "xsltproc --output %s /usr/share/xml/docbook/stylesheet/nwalsh/fo/docbook.xsl %s")

;; 导出html时,嵌入图片,而不是创建图片的链接
(setq org-html-inline-images t)
;; 导出时不转仪"_"和"^"
(setq org-export-with-sub-superscripts nil)
;; 设置导出时,每个页面使用指定的样式css
;; (setq org-html-head-extra "<link rel=\"stylesheet\" href=\"http://doc.norang.ca/org.css\" type=\"text/css\" />")
;; 不使用默认的样式
;; (setq org-html-head-include-default-style nil)
;; Do not generate internal css formatting for HTML exports
;; (setq org-export-htmlize-output-type 'css)
;; Export with LaTeX fragments
;; (setq org-export-with-LaTeX-fragments t)
;; 设置导出的级别
(setq org-export-headline-levels 6)

;; List of projects
;; my_note       - http://www.norang.ca/
;; my_gtd          - miscellaneous todo lists for publishing
(setq org-publish-project-alist
      `(("my_note"
               :base-directory ,MY-NOTE-PATH ;导出的源代码路径
               :publishing-directory "/var/www/my_note" ;导出的目的代码路径
               :recursive t
               :table-of-contents nil
               :base-extension "org"    ;只导出.org文件
               :publishing-function org-html-publish-to-html
               :style-include-default t
               :section-numbers nil
               :table-of-contents nil
               :auto-sitemap t
               :sitemap-filename "index.html"
               :sitemap-title "My NOTE"
               :sitemap-style "tree"
               ;; :html-head "<link rel=\"stylesheet\" href=\"norang.css\" type=\"text/css\" />"
               ;; :author-info nil
               ;; :creator-info nil
               )
              ("my_gtd"
               :base-directory ,MY-GTD-PATH
               :publishing-directory "/var/www/my_gtd" ;导出的目的代码路径
               :recursive nil
               :section-numbers nil
               :table-of-contents nil
               :base-extension "org"
               :publishing-function (org-html-publish-to-html org-org-publish-to-org)
               :style-include-default t
               :auto-sitemap t
               :sitemap-filename "index.html"
               :sitemap-title "My GTD"
               :sitemap-style "tree"
               ;; :html-head "<link rel=\"stylesheet\" href=\"/org.css\" type=\"text/css\" />"
               ;; :author-info nil
               ;; :creator-info nil
               )))

; I'm lazy and don't want to remember the name of the project to publish when I modify
; a file that is part of a project.  So this function saves the file, and publishes
; the project that includes this file
;
; It's bound to C-S-F12 so I just edit and hit C-S-F12 when I'm done and move on to the next thing
(defun bh/save-then-publish (&optional force)
  (interactive "P")
  (save-buffer)
  (org-save-all-org-buffers)
  (let ((org-html-head-extra)
        (org-html-validation-link "<a href=\"http://validator.w3.org/check?uri=referer\">Validate XHTML 1.0</a>"))
    (org-publish-current-project force)))

(global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)

(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)

;; Make babel results blocks lowercase
;; (setq org-babel-results-keyword "results")

(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

;; 设置可以load的代码块
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (dot . t)
   (ditaa . t)
   ;; (R . t)
   (python . t)
   (ruby . t)
   (gnuplot . t)
   (clojure . t)
   (shell . t)
   (ledger . t)
   (org . t)
   (plantuml . t)
   (latex . t)
   (elasticsearch . t)))

;; C-c C-c执行代码块时,不需要确认
(setq org-confirm-babel-evaluate nil)

;; 当使用C-c'编辑sqC代码块时,使用c++ mode
(add-to-list 'org-src-lang-modes '("sqC" . c++))

(require 'org-crypt)
;; 保存前,自动为headline加密
(org-crypt-use-before-save-magic)
;; 设置crypt标签不参与继承,这样就避免出现加密数据中包含加密数据的情况,由于每层加密数据都需要输一次密码解密,这样显得太麻烦了.
(setq org-tags-exclude-from-inheritance  '("crypt"))
;; GPG key to use for encryption
;; 设置默认的org加密密钥,可以被"CRYPTKEY" property的值所覆盖
;; (setq org-crypt-key "passwd")

(setq org-crypt-disable-auto-save t)

;; 由于开起了evil-mode,基本上speed commands key都被evil-mode下的key所覆盖了,所以作用不大
;; (setq org-use-speed-commands t)

(setq org-link-mailto-program '(compose-mail "%a" "%s"))

(require 'org-mime nil t)

(add-hook 'message-mode-hook 'orgstruct++-mode 'append)
(add-hook 'message-mode-hook 'turn-on-auto-fill 'append)
;; (add-hook 'message-mode-hook 'bbdb-define-all-aliases 'append)
(add-hook 'message-mode-hook 'orgtbl-mode 'append)
;; (add-hook 'message-mode-hook 'turn-on-flyspell 'append)
(add-hook 'message-mode-hook
          '(lambda () (setq fill-column 72))
          'append)

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/is-habit-p ()
  "Any task with a habit tag"
  (save-restriction
    (widen)
    (member "habit" (org-get-tags))
    ))
(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defvar bh/hide-scheduled-and-waiting-next-tasks t)

(defun bh/toggle-next-task-display ()
  (interactive)
  (setq bh/hide-scheduled-and-waiting-next-tasks (not bh/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun bh/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((bh/is-project-p)
            nil)
           ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((bh/is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-habit-p)
        next-headline)
       ((and bh/hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((bh/is-project-p)
        next-headline)
       ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun bh/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((bh/is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (bh/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((bh/is-habit-p)
        subtree-end)
       ((bh/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((bh/is-habit-p)
        subtree-end)
       ((and (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (bh/is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((bh/is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
        nil
      next-headline)))

(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-number (format-time-string "%d" (current-time))))
                     (a-month-ago (* 60 60 24 (+ daynr 1)))
                     (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                     (this-month (format-time-string "%Y-%m-" (current-time)))
                     (subtree-is-current (save-excursion
                                           (forward-line 1)
                                           (and (< (point) subtree-end)
                                                (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                (if subtree-is-current
                    subtree-end ; Has a date in this month or last month, skip it
                  nil))  ; available to archive
            (or subtree-end (point-max)))
        next-headline))))

(setq org-confirm-elisp-link-not-regexp "")
(setq org-confirm-shell-link-not-regexp "^mstsc")

(defun org-open-at-point-without-shell-output-buffer-advise (ori_fn &rest args)
  ""
  (save-window-excursion
    (apply ori_fn args)))

(advice-add 'org-open-at-point :around #'org-open-at-point-without-shell-output-buffer-advise)

(require 'comint)
(org-add-link-type "ssh" 'org-ssh-open)
(defcustom org-ssh-command "putty"
  "The ssh client to be used to login remote server"
  :group 'org-link
  :type '(choice (const "putty") (const "ssh") (const "xshell")))

(defun org-ssh-open (path)
  "login remote server
PATH should be a topic that can be thrown at the ssh client"
  (make-comint org-ssh-command org-ssh-command nil (replace-regexp-in-string "^//" "" path)))

;; (setq org-show-following-heading t)
;; (setq org-show-hierarchy-above t)
;; (setq org-show-siblings '((default)))

(setq org-list-allow-alphabetical t)

(setq org-list-demote-modify-bullet '(("+" . "-")
                                      ("*" . "-")
                                      ("1." . "-")
                                      ("1)" . "-")
                                      ("A)" . "-")
                                      ("B)" . "-")
                                      ("a)" . "-")
                                      ("b)" . "-")
                                      ("A." . "-")
                                      ("B." . "-")
                                      ("a." . "-")
                                      ("b." . "-")))

;; 高亮显示code blocks
(setq org-src-fontify-natively t)

(setq org-file-apps '((auto-mode . emacs)
                      ("\\.mm\\'" . system)
                      ("\\.x?html?\\'" . system)
                      ("\\.pdf\\'" . system)
                      (t . system)))

(add-to-list 'org-structure-template-alist '("se" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC" "<src lang=\"emacs-lisp\">\n?\n</src>"))

(setq org-catch-invisible-edits 'smart)

(require 'subr-x)
(defun get-category-from-path (path)
  "根据路径生成类别"
  (let* ((dir (file-relative-name (file-name-directory path)
                                  MY-NOTE-PATH)))
    (if (file-in-directory-p (file-name-directory path) MY-NOTE-PATH)
        (string-join (remove "" (split-string dir "[/\\]")) ", ")
      (if (string-match ".+[/\\]\\([^/\\]+\\)[/\\]$" dir) ;若不在"我的笔记目录下",则取其所在的目录名称为类别
          (match-string 1 dir)
        dir) )))

(defun new-org-file-init ()
  "init new org file template"
  (interactive)
  (when (equal "org" (file-name-extension buffer-file-name))
    (insert (concat "#+TITLE: "(file-name-base buffer-file-name)) "\n")
    (insert "#+AUTHOR: " user-login-name "\n")
    (insert "#+TAGS: "  (get-category-from-path buffer-file-name)"\n")
    (insert "#+DATE: " (format-time-string "[%Y-%m-%d %a %H:%M]" (current-time)) "\n")
    (insert "#+LANGUAGE:  zh-CN\n")
    (insert "#+OPTIONS:  H:6 num:nil toc:t \\n:nil ::t |:t ^:nil -:nil f:t *:t <:nil")))
(add-to-list 'find-file-not-found-hooks 'new-org-file-init)

;; 设置org笔记时的缩进
;; (setq org-description-max-ident 5)
;; (require 'org-realtime-preview)         ;写笔记时,可以开启实时预览
(require 'org-helper)

(require 'org-mobile)

(setq org-mobile-directory "~/mobileorg")

;; (setq org-mobile-files '("~/我的GTD/office.org"
;;                              "~/我的GTD/home.org"
;;                              ))

(setq org-directory MY-GTD-PATH)

(setq org-mobile-inbox-for-pull (concat (file-name-as-directory MY-GTD-PATH) "from-mobile.org"))

(defcustom org-mobile-checksum-binary (or (executable-find "md5sum"))
 "Executable used for computing checksums of agenda files."
 :group 'org-mobile
 :type 'string)

(provide 'init-GTD-org)
