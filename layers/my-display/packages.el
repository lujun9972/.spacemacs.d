;;; packages.el --- my-display Layer packages File for Spacemacs
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
(setq my-display-packages
    '(
      ;; package names go here
      fullframe
      ;; shrface
      shr-tag-pre-highlight
      ))

;; List of packages to exclude.
(setq my-display-excluded-packages '())

;; For each package, define a function my-display/init-<package-name>
;;
;; ;; fullframe可以一个fullframe中执行某些命令,并在显示之前前保存window-configuration,当命令完成后,会还原该window-configuration
(defun my-display/init-fullframe ()
  "Initialize my package"
  (use-package fullframe
    :config
    (fullframe list-packages quit-window)	;执行list-packages命令会全屏显示,执行quit-window后会自动还原原window配置信息.
    (fullframe magit-status magit-mode-quit-window nil)))

(defun my-display/init-shrface ()
  "Initialize my package"
  (use-package shrface
    :after shr
    :config
    (with-eval-after-load 'shr
      (require 'shrface)
      (shrface-basic) ; enable shrfaces, must be called before loading eww/dash-docs/nov.el
      (shrface-trial)) ; enable shrface experimental face(s), must be called before loading eww/dash-docs/nov.el
    (with-eval-after-load 'eww
      (add-hook 'eww-after-render-hook 'shrface-mode)) ; this will affect eww and dash-docs
    (with-eval-after-load 'nov
      (setq nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions))
      (add-hook 'nov-mode-hook 'shrface-mode))
    ))

(defun my-display/init-shr-tag-pre-highlight ()
  "Initialize my package"
  (use-package shr-tag-pre-highlight
    :ensure t
    :after shr
    :config
    (add-to-list 'shr-external-rendering-functions
                 '(pre . shr-tag-pre-highlight))
    (when (version< emacs-version "26")
      (with-eval-after-load 'eww
        (advice-add 'eww-display-html :around
                    'eww-display-html--override-shr-external-rendering-functions)))))

;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
