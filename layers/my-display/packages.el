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
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
