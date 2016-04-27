;;; packages.el --- my-edit Layer packages File for Spacemacs
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
(setq my-edit-packages
    '(
      ;; package names go here
      wgrep
      ;; org-eww
      ))

;; List of packages to exclude.
(setq my-edit-excluded-packages '())

;; For each package, define a function my-edit/init-<package-name>
;;
;; 使用wgrep可以直接修改grep buffer,并将修改映射回原文件
(defun my-edit/init-wgrep ()
  "Initialize my package"
  (use-package wgrep
    :defer t))

(defun my-edit/init-org-eww ()
  "Initialize my package"
  (with-eval-after-load 'org (use-package org-eww
                               )))
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
