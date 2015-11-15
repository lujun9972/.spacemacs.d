;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun filter-valid-files (&rest files)
  "该函数接收一系列的文件路径,并以列表的形式抽取出其中存在的文件路径"
  (remove-if-not #'file-exists-p files))
(defun filter-valid-file (&rest files)
  "该函数接收一系列的文件路径,并返回第一个存在的文件路径."
  (car (apply #'filter-valid-files files)))

(defun obj-to-symbol (obj)
  "转换为symbol"
  (unless (stringp obj)
	(setq obj (prin1-to-string obj t)))
  (intern obj))

(defun get-load-or-default-directory()
  (file-name-directory (or load-file-name default-directory)))

(defun org-babel-tangle-newer-file (FILE &optional TARGET-FILE LANG)
  "tangle only if the file is newer than the target-file"
  (let ((age (lambda (file)
			   (float-time
				(time-subtract (current-time)
							   (nth 5 (or (file-attributes (file-truename file))
										  (file-attributes file))))))))
	(unless (and TARGET-FILE
				 (file-exists-p TARGET-FILE)
				 (> (funcall age FILE) (funcall age TARGET-FILE)))
	  (setq TARGET-FILE
			(car (org-babel-tangle-file FILE TARGET-FILE LANG))))
	TARGET-FILE))

(defun org-babel-tangle-newer-elisp-file (FILE)
  "tangle only if the org-mode file is newer than the elisp-file"
  (let ((TARGET-FILE (concat (file-name-sans-extension FILE) ".el"))
		(LANG "emacs-lisp"))
	(org-babel-tangle-newer-file FILE TARGET-FILE LANG)))

(defun level-require (sublevel &optional level-load-path)
  "加载下一层次的配置信息
配置文件被划分为不同层次,不同层次间用-来划分. 例如init-program-lisp.el就是init-program.el的下一层次
因此,在init-program.el中要加载init-program-lisp.el只需要(level-load \"lisp.el\"即可"
  (message "enter level-require %s" sublevel)
  (unless level-load-path
	(setq level-load-path (get-load-or-default-directory)))
  (add-to-list 'load-path level-load-path)
  (let (level)
	(setq level (file-name-sans-extension (file-name-base  (or load-file-name
															   (buffer-file-name)
															   "~/emacs-init/init.el"))))
	(setq level (concat level "-" sublevel))
	(cond ((file-exists-p (expand-file-name (concat level ".org") level-load-path))
		   (org-babel-tangle-newer-elisp-file (expand-file-name (concat level ".org") level-load-path)))
		  ((file-exists-p (expand-file-name (concat sublevel ".org") level-load-path))
		   (org-babel-tangle-newer-elisp-file (expand-file-name (concat sublevel ".org") level-load-path))))
	(or
	 (require (obj-to-symbol level)nil t)
	 (require (obj-to-symbol sublevel) nil t)))
  (message "leave level-require %s" sublevel))


(defun level-load (sublevel &optional level-load-path)
  "加载下一层次的配置信息
配置文件被划分为不同层次,不同层次间用-来划分. 例如init-program-lisp.el就是init-program.el的下一层次
因此,在init-program.el中要加载init-program-lisp.el只需要(level-load \"lisp.el\"即可"
  (message "enter level-load %s" sublevel)
  (unless level-load-path
	(setq level-load-path (get-load-or-default-directory)))
  (add-to-list 'load-path level-load-path)
  (let (level)
	(setq level (file-name-sans-extension (file-name-base  (or load-file-name
															   (buffer-file-name)
															   "~/emacs-init/init.el"))))
	(setq level (concat level "-" sublevel))
	(or
	 (cond ((file-exists-p (expand-file-name (concat level ".org") level-load-path))
			(org-babel-load-file (expand-file-name (concat level ".org") level-load-path) t))
		   ((file-exists-p (expand-file-name (concat sublevel ".org") level-load-path))
			(org-babel-load-file (expand-file-name (concat sublevel ".org") level-load-path) t)))
	 (load level t)
	 (load sublevel t)))
  (message "leave level-load %s" sublevel))
(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path (filter-valid-files "d:/spacemacs-private")
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     auto-completion
     better-defaults
     smex
     (deft :variables
       deft-recursive t
       deft-directory MY-NOTE-PATH)
     finance
     gtags
     semantic
     games
     emoji
     emacs-lisp
     ;; git
     markdown
     clojure
     emacs-lisp
     c-c++
     ;; ruby
     sql
     gnus
     (erc :variables
          erc-nick "lujun9972"  		;设置昵称
          erc-user-full-name "lujunwei" ;设置全称
          erc-email-userid "lujun9972@gmail.com"  
          ;; erc-server "irc.freenode.net"  
          ;; erc-port "6667"  

          erc-autojoin-channels-alist	;设置自动登录的频道
          '(("freenode.net" "ubuntu-cn")  
            ("oftc.net" "#emacs-cn"))  

          erc-keywords '("emacs" "lisp") ;高亮消息中的关键字
          erc-pals '("rms")  

          erc-format-nick-function 'erc-format-@nick  
          erc-interpret-mirc-color t	;支持mIRC风格的颜色命令
          erc-button-buttonize-nicks nil  
          erc-track-position-in-mode-line 'before-modes
          erc-encoding-coding-alist '(("#linuxfire" . chinese-iso-8bit))
          erc-ignore-list nil)
     version-control
     ;; org
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     syntax-checking
     version-control
     my-display
     my-misc
     my-edit
     my-GTD
     my-file
     my-eshell
     my-program
     my-life
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages nil))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'hybrid
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light
                         solarized-light
                         solarized-dark
                         leuven
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido t
   ;; If non nil, `helm' will try to miminimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any
user code."
  ;; 定义几个常用目录的路径
  (defconst MY-LISP-PATH (filter-valid-file "~/MyLisp" "e:/MyLisp" "d:/MyLisp"))
  (defconst CODE-LIBRARY-PATH (filter-valid-file "~/CodeLibrary/elisp.org" "d:/CodeLibrary/elisp.org"  "e:/CodeLibrary/elisp.org"))
  (defconst MY-GTD-PATH (filter-valid-file "~/我的GTD/" "e:/我的GTD/" "d:/我的GTD/"))
  (defconst MY-NOTE-PATH (filter-valid-file "~/我的笔记" "e:/我的笔记" "d:/我的笔记"))
  ;; 加载相关辅助函数
  (add-to-list 'load-path MY-LISP-PATH)
  (dolist (helper-package (directory-files MY-LISP-PATH nil "helper\.el"))
    (require (intern (file-name-base helper-package)))))

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
 This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."
)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (org2blog w3m emms diff-hl gist yasnippet projectile smart-compile flymake-cppcheck dos lua-mode deferred lispy zeal-at-point ws-butler wgrep switch-window sunrise-x-w32-addons sunrise-x-tree sunrise-x-tabs sunrise-x-popviewer sunrise-x-modeline sunrise-x-loop sunrise-x-checkpoints sunrise-x-buttons sr-speedbar smex slime showkey redshank paredit markdown-mode magit keyfreq info+ idomenu ido-ubiquitous ibuffer-vc git-timemachine ggtags fullframe evil discover-my-major discover dired-sort dired+ dictionary deft clean-aindent-mode bbdb auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button)))) t))
