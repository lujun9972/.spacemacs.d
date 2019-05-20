;;; packages.el --- my-misc Layer packages File for Spacemacs
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
(setq my-misc-packages
    '(
      ;; package names go here
      ;; (ido :location built-in)
      ;; smex
      (desktop :location built-in)
      ;; tabbar
      uimage
      keyfreq
      ;; sr-speedbar
      ibuffer
      ibuffer-vc
      ;; showkey
      dmenu
      start-menu
      clean-buffers
      (pocket-mode :location (recipe
                              :fetcher github
                              :repo "lujun9972/pocket-mode"))
      es-mode
      tablist
      ))

;; List of packages to exclude.
(setq my-misc-excluded-packages '())

;; For each package, define a function my-misc/init-<package-name>
;;

;; 开启ido-mode
;; (defun my-misc/post-init-ido ()
;;   (use-package ido
;;     :config
;;     (ido-mode 'both)
;;     (setq ido-enable-prefix nil
;;           ido-enable-flex-matching t 
;;           ido-everywhere t
;;           ido-max-directory-size 100000
;;           ido-create-new-buffer 'always
;;           ;; Use the current window when visiting files and buffers with ido
;;           ido-default-file-method 'selected-window
;;           ido-default-buffer-method 'selected-window
;;           ido-use-filename-at-point nil
;;           ido-auto-merge-work-directories-length 0
;;           ido-use-virtual-buffers t)
;;     (add-to-list 'ido-ignore-directories "\\`\\.git/")
;;     (add-to-list 'ido-ignore-files "\\~\\`")
;;     (add-to-list 'ido-ignore-files "\\.doc\\`")
;;     ;; 若打开文件没有权限,自动使用sudo方式打开
;;     (defun alternate-current-file-as-root (&rest args)
;;       "以sudo方式打开当前buffer文件"
;;       (interactive)
;;       (let ((file (buffer-file-name)))
;;         (when (and file
;;                    (not (file-writable-p file))
;;                    (not (file-remote-p file))
;;                    (y-or-n-p-with-timeout "是否使用sudo方式打开当前文件" 10 "n"))
;;           (find-alternate-file (concat "/sudo::" file)))))
;;     (advice-add 'ido-find-file :after #'alternate-current-file-as-root)))

;; (defun my-misc/init-ido-ubiquitous ()
;;     (use-package ido-ubiquitous
;;       :ensure t
;;       :config
;;       (ido-ubiquitous-mode t)))

;; (defun my-misc/init-smex ()
;;   (use-package smex
;;     :ensure t
;;     :config
;;     (smex-initialize)
;;     :bind (("M-x" . smex)
;;            ("C-x x" . smex)
;;            ("M-X" . smex-major-mode-commands))))

(defun my-misc/post-init-desktop ()
  (use-package desktop
    :config
    (setq desktop-path '("."))
    ;; 为每个不同的系统定义不同的保存名称,这是为了使得同一台机器上不同环境间互补干扰
    (setq desktop-base-file-name (file-name-nondirectory (format "%s.emacs.desktop" system-type)))
    (setq desktop-auto-save-timeout 600)	;10分钟保存一次
    ;; (desktop-save-mode 1)
    (toggle-save-place-globally 1)			;保存各buffer的光标位置
    (savehist-mode t)						;保存minibuffer的history
    ))

(defun my-misc/init-tabbar ()
  "Initialize my package"
  (use-package tabbar
    :config
    (tabbar-mode t)
    (defvar tabbar-buffer-name-group-alist
      '(("*tramp.+*" "hidden"))
      "根据名称的正则表达式来为tabbar提供分组信息

每个原始的car为buffer-name匹配的正则表达式, cdr为分组list")

    (defun my-tabbar-buffer-groups ()
      "tabbar分组函数

先根据`tabbar-buffer-name-group-alist'与buffer-name查找匹配的分组,否则使用`tabbar-buffer-groups'进行分组"
      (let ((match-rule (find-if (lambda (match-rule)
                                   (string-match-p (car match-rule) (buffer-name))) tabbar-buffer-name-group-alist)))
        (if match-rule
            (cdr match-rule)
          (tabbar-buffer-groups))))

    (setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)))

(defun my-misc/init-uimage ()
  (use-package uimage
    :ensure t))

;; 配置keyfreq用来记录按键频率,使用keyfre-show查看
(defun my-misc/init-keyfreq ()
  (use-package keyfreq
    :config
    (keyfreq-mode 1)
    (keyfreq-autosave-mode 1)))

;; (defun my-misc/init-sr-speedbar ()
;;   (use-package sr-speedbar
;;     :init
;;     (defalias 'ad-advised-definition-p #'ad-advice-p)
;;     :config
;;     (setq sr-speedbar-skip-other-window-p t)
;;     (setq sr-speedbar-right-side nil)))

(defun my-misc/init-ibuffer ()
  (use-package ibuffer
    :init
    (defun ibuffer-set-up-preferred-filters ()
      (ibuffer-vc-set-filter-groups-by-vc-root)
      (unless (eq ibuffer-sorting-mode 'filename/process)
        (ibuffer-do-sort-by-filename/process)))

    (add-hook 'ibuffer-hook 'ibuffer-set-up-preferred-filters)
    (setq-default ibuffer-show-empty-filter-groups nil)
    ;; Modify the default ibuffer-formats (toggle with `)
    (setq ibuffer-formats
          '((mark modified read-only vc-status-mini " "
                  (name 18 18 :left :elide)
                  " "
                  (size-h 9 -1 :right)
                  " "
                  (mode 16 16 :left :elide)
                  " "
                  filename-and-process)
            (mark modified read-only vc-status-mini " "
                  (name 18 18 :left :elide)
                  " "
                  (size-h 9 -1 :right)
                  " "
                  (mode 16 16 :left :elide)
                  " "
                  (vc-status 16 16 :left)
                  " "
                  filename-and-process)))

    (setq ibuffer-filter-group-name-face 'font-lock-doc-face)
    (global-set-key (kbd "C-x C-b") 'ibuffer)
    :defer t
    :config
    (fullframe ibuffer ibuffer-quit)
    ;; Use human readable Size column instead of original one
    (define-ibuffer-column size-h
      (:name "Size" :inline t)
      (cond
       ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
       ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
       (t (format "%8d" (buffer-size)))))
    ))

(defun my-misc/init-ibuffer-vc ()
  (use-package ibuffer-vc))



;; showkey可以显示操作的按及运行的函数
(defun my-misc/init-showkey ()
  (use-package showkey
    :defer t))

(defun my-misc/init-dmenu ()
  (use-package dmenu
    :defer t
    :bind ("s-r" . dmenu)))

(defun my-misc/init-clean-buffers ()
  (use-package clean-buffers
    :config
    (clean-buffers-turn-on-auto-clean-buffers)
    ))



(defun my-misc/init-start-menu ()
  (use-package start-menu
    :config
    (start-menu-enable)
    (global-set-key (kbd "<mouse-2>") 'start-menu-popup)
    (evil-local-set-key 'normal (kbd "<mouse-2>") 'start-menu-popup)))
(defun my-misc/init-pocket-mode ()
  (use-package pocket-mode
    :defer  t
    :config
    (setq pocket-items-per-page 30)
    (setq pocket-auto-refresh t)
    (eval-after-load 'pocket-mode
      '(require 'url2org))))

(defun my-misc/init-es-mode ()
  (use-package es-mode
    :defer  t
    :config
    (setq es-always-pretty-print t)))

(defun my-misc/init-tablist ()
  (use-package tablist
    :defer  t
    :config
    (add-hook 'tabulated-list-mode-hook 'tablist-minor-mode)
    (with-eval-after-load 'tablist
      (easy-menu-define my-tablist-minor-mode-map-misc-menu tablist-minor-mode-map "Menu for Tablist Minor Mode Map."
        '("Tablist Misc"
          ["Sort" tablist-sort :help "(tablist-sort &optional COLUMN)\n\nSort the tabulated-list by COLUMN.\n\nCOLUMN may be either a name or an index.  The default compare\nfunction is given by the `tabulated-list-format', which see.\n\nThis function saves the current sort column and the inverse\nsort-direction in the variable `tabulated-list-sort-key', which\nalso determines the default COLUMN and direction.\n\nThe main difference to `tabulated-list-sort' is, that this\nfunction sorts the buffer in-place and it ignores a nil sort\nentry in `tabulated-list-format' and sorts on the column\nanyway (why not ?)."]
          ["Do Kill Lines" tablist-do-kill-lines :help "(tablist-do-kill-lines &optional ARG INTERACTIVE)\n\nRemove ARG lines from the display."]
          "--"
          ["Export Csv" tablist-export-csv :help "(tablist-export-csv &optional SEPARATOR ALWAYS-QUOTE-P INVISIBLE-P OUT-BUFFER DISPLAY-P)\n\nExport a tabulated list to a CSV format.\n\nUse SEPARATOR (or ;) and quote if necessary (or always if\nALWAYS-QUOTE-P is non-nil).  Only consider non-filtered entries,\nunless invisible-p is non-nil.  Create a buffer for the output or\ninsert it after point in OUT-BUFFER.  Finally if DISPLAY-P is\nnon-nil, display this buffer.\n\nReturn the output buffer."]
          "--"
          ["Previous Line" tablist-previous-line :help "(tablist-previous-line &optional N)"]
          ["Next Line" tablist-next-line :help "(tablist-next-line &optional N)"]
          "--"
          ["Revert" tablist-revert :help "(tablist-revert)\n\nRevert the list with marks preserved, position kept."]
          ["Quit" tablist-quit :help "(tablist-quit)"]))

      (easy-menu-define my-tablist-minor-mode-map-mark-menu tablist-minor-mode-map "Menu for Tablist Minor Mode Map."
        '("Tablist Mark"
          ["Mark Items Regexp" tablist-mark-items-regexp :help "(tablist-mark-items-regexp COLUMN-NAME REGEXP)\n\nMark entries matching REGEXP in column COLUMN-NAME."]
          ["Mark Items Numeric" tablist-mark-items-numeric :help "(tablist-mark-items-numeric BINOP COLUMN-NAME OPERAND)\n\nMark items fulfilling BINOP with arg OPERAND in column COLUMN-NAME.\n\nFirst the column's value is coerced to a number N.  Then the test\nproceeds as (BINOP N OPERAND)."]
          "--"
          ["Mark Forward" tablist-mark-forward :help "(tablist-mark-forward &optional ARG INTERACTIVE)\n\nMark ARG entries forward.\n\nARG is interpreted as a prefix-arg.  If interactive is non-nil,\nmaybe use the active region instead of ARG.\n\nSee `tablist-put-mark' for how entries are marked."]
          ["Unmark Forward" tablist-unmark-forward :help "(tablist-unmark-forward &optional ARG INTERACTIVE)\n\nUnmark ARG entries forward.\n\nSee `tablist-mark-forward'."]
          ["Unmark Backward" tablist-unmark-backward :help "(tablist-unmark-backward &optional ARG INTERACTIVE)\n\nUnmark ARG entries backward.\n\nSee `tablist-mark-forward'."]
          "--"
          ["Change Marks" tablist-change-marks :help "(tablist-change-marks OLD NEW)\n\nChange all OLD marks to NEW marks.\n\nOLD and NEW are both characters used to mark files."]
          "--"
          ["Toggle Marks" tablist-toggle-marks :help "(tablist-toggle-marks)\n\nUnmark all marked and mark all unmarked entries.\n\nSee `tablist-put-mark'."]
          ["Unmark All Marks" tablist-unmark-all-marks :help "(tablist-unmark-all-marks &optional MARKS INTERACTIVE)\n\nRemove alls marks in MARKS.\n\nMARKS should be a string of mark characters to match and defaults\nto all marks.  Interactively, remove all marks, unless a prefix\narg was given, in which case ask about which ones to remove.\nGive a message, if interactive is non-nil.\n\nReturns the number of unmarked marks."]))

      (easy-menu-define my-tablist-minor-mode-map-filter-menu tablist-minor-mode-map "Menu for Tablist Minor Mode Map."
        '("Tablist Filter"
          ["Push Regexp Filter" tablist-push-regexp-filter :help "(tablist-push-regexp-filter COLUMN-NAME REGEXP)\n\nAdd a new filter matching REGEXP in COLUMN-NAME.\n\nThe filter is and'ed with the current filter.  Use\n`tablist-toggle-first-filter-logic' to change this."]
          ["Push Equal Filter" tablist-push-equal-filter :help "(tablist-push-equal-filter COLUMN-NAME STRING)\n\nAdd a new filter whre string equals COLUMN-NAME's value.\n\nThe filter is and'ed with the current filter.  Use\n`tablist-toggle-first-filter-logic' to change this."]
          ["Push Numeric Filter" tablist-push-numeric-filter :help "(tablist-push-numeric-filter OP COLUMN-NAME 2ND-ARG)\n\nAdd a new filter matching a numeric predicate.\n\nThe filter is and'ed with the current filter.  Use\n`tablist-toggle-first-filter-logic' to change this."]
          ["Pop Filter" tablist-pop-filter :help "(tablist-pop-filter &optional N INTERACTIVE)\n\nRemove the first N filter components."]
          "--"
          ["Negate Filter" tablist-negate-filter :help "(tablist-negate-filter &optional INTERACTIVE)\n\nNegate the current filter."]
          ["Suspend Filter" tablist-suspend-filter :style toggle :selected tablist-filter-suspended :help "(tablist-suspend-filter &optional FLAG)\n\nTemporarily disable filtering according to FLAG.\n\nInteractively, this command toggles filtering."]
          ["Clear Filter" tablist-clear-filter :help "(tablist-clear-filter)"]
          ["Toggle First Filter Logic" tablist-toggle-first-filter-logic :help "(tablist-toggle-first-filter-logic)\n\nToggle between and/or for the first filter operand."]
          ["Display Filter" tablist-display-filter :style toggle :selected (assq 'tablist-display-filter-mode-line-tag mode-line-format) :help "(tablist-display-filter &optional FLAG)\n\nDisplay the current filter according to FLAG.\n\nIf FLAG has the value 'toggle, toggle it's visibility.\nIf FLAG has the 'state, then do nothing but return the current\nvisibility."]
          ["Edit Filter" tablist-edit-filter :help "(tablist-edit-filter)"]
          "--"
          ["Name Current Filter" tablist-name-current-filter :help "(tablist-name-current-filter NAME)"]
          ["Push Named Filter" tablist-push-named-filter :help "(tablist-push-named-filter NAME)\n\nAdd a named filter called NAME.\n\nNamed filter are saved in the variable `tablist-named-filter'."]
          ["Delete Named Filter" tablist-delete-named-filter :help "(tablist-delete-named-filter NAME &optional MODE)"]
          ["Deconstruct Named Filter" tablist-deconstruct-named-filter :help "(tablist-deconstruct-named-filter)"]))

      (easy-menu-define my-tablist-minor-mode-map-column-menu tablist-minor-mode-map "Menu for Tablist Minor Mode Map."
        '("Tablist Column"
          ["Forward Column" tablist-forward-column :help "(tablist-forward-column N)\n\nMove n columns forward, while wrapping around."]
          ["Backward Column" tablist-backward-column :help "(tablist-backward-column N)\n\nMove n columns backward, while wrapping around."]
          "--"
          ["Move To Major Column" tablist-move-to-major-column :help "(tablist-move-to-major-column &optional FIRST-SKIP-INVISIBLE-P)\n\nMove to the first major column."]
          "--"
          ["Shrink Column" tablist-shrink-column :help "(tablist-shrink-column &optional COLUMN WIDTH)"]
          ["Enlarge Column" tablist-enlarge-column :help "(tablist-enlarge-column &optional COLUMN WIDTH)\n\nEnlarge column COLUMN by WIDTH.\n\nThis function is lazy and therfore pretty slow."])))
    ))
