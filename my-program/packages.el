;;; packages.el --- my-program Layer packages File for Spacemacs
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
(setq my-program-packages
    '(
      ;; package names go here
      ws-butler
      git-timemachine
      zeal-at-point
      lispy
      gist
      smart-compile
      elog
      code-library
      ))

;; List of packages to exclude.
(setq my-program-excluded-packages '())

;; For each package, define a function my-program/init-<package-name>
;;
;; ws-butler helps managing whitespace on every line of code written or edited, in an unobtrusive, help you write clean code without noisy whitespace effortlessly. 
(defun my-program/init-ws-butler ()
  "Initialize my package"
  (use-package ws-butler
    :config
    (add-hook 'c-mode-common-hook 'ws-butler-mode)))

(defun my-program/init-git-timemachine ()
  (use-package git-timemachine))

;; 使用zeal查看docset
(defun my-program/init-zeal-at-point ()
  (use-package zeal-at-point
    :config
    (global-set-key "\C-ch" 'zeal-at-point)
    (add-to-list 'exec-path (filter-valid-file "C:/Program Files/Zeal" "/cygdrive/c/Program Files/Zeal/"))
    ;; (add-to-list 'zeal-at-point-mode-alist '(perl-mode . "perl"))
    ;; (add-hook 'rinari-minor-mode-hook
    ;;    (lambda () (setq zeal-at-point-docset "rails")))
    ))

(defun my-program/init-lispy ()
  (use-package lispy
    :config
    (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
    (add-hook 'lisp-mode-hook (lambda () (lispy-mode 1)))
    (add-hook 'clojure-mode-hook (lambda () (lispy-mode 1)))));;

(defun my-program/init-gist ()
  (use-package gist))

(defun my-program/init-smart-compile ()
  (use-package smart-compile
    :config
    (defun set-compile-key()
      (local-set-key (kbd "<f5>") (lambda ()
                                    (interactive)
                                    (if (buffer-live-p last-fail-compilation-buffer)
                                        (recompile)
                                      (smart-compile 1)))))

    (add-hook 'prog-mode-hook #'set-compile-key)

    ;; 编译sendtonps
    (add-to-list 'smart-compile-alist
                 '("sendnps.+\.cpp$" . "plink cnaps2@10.8.6.10 \". ~/.profile;cd app/nps;make sendtonps\""))
    (add-to-list 'smart-compile-alist
                 '("sendnps.+\.h$" . "plink cnaps2@10.8.6.10 \". ~/.profile;cd app/nps;make sendtonps\""))

    ;; 编译recvfromnps
    (add-to-list 'smart-compile-alist
                 '("recvnps.+\.cpp$" . "plink cnaps2@10.8.6.10 \". ~/.profile;cd app/nps;make recvfromnps\""))
    (add-to-list 'smart-compile-alist
                 '("recvnps.+\.h$" . "plink cnaps2@10.8.6.10 \". ~/.profile;cd app/nps;make recvfromnps\""))

    ;; 编译sendtobeps
    (add-to-list 'smart-compile-alist
                 '("sendbeps.+\.cpp$" . "plink cnaps2@10.8.6.10 \". ~/.profile;cd app/beps;make sendtobeps\""))
    (add-to-list 'smart-compile-alist
                 '("sendbeps.+\.h$" . "plink cnaps2@10.8.6.10 \". ~/.profile;cd app/beps;make sendtobeps\""))

    ;; 编译recvfrombeps
    (add-to-list 'smart-compile-alist
                 '("recvbeps.+\.cpp$" . "plink cnaps2@10.8.6.10 \". ~/.profile;cd app/beps;make recvfrombeps\""))
    (add-to-list 'smart-compile-alist
                 '("recvbeps.+\.h$" . "plink cnaps2@10.8.6.10 \". ~/.profile;cd app/beps;make recvfrombeps\""))

    ;; 编译sendtohvps
    (add-to-list 'smart-compile-alist
                 '("sendhvps.+\.cpp$" . "plink cnaps2@10.8.6.10 \". ~/.profile;cd app/hvps;make sendtohvps\""))
    (add-to-list 'smart-compile-alist
                 '("sendhvps.+\.h$" . "plink cnaps2@10.8.6.10 \". ~/.profile;cd app/hvps;make sendtohvps\""))

    ;; 编译recvfromhvps
    (add-to-list 'smart-compile-alist
                 '("recvhvps.+\.cpp$" . "plink cnaps2@10.8.6.10 \". ~/.profile;cd app/hvps;make recvfromhvps\""))
    (add-to-list 'smart-compile-alist
                 '("recvhvps.+\.h$" . "plink cnaps2@10.8.6.10 \". ~/.profile;cd app/hvps;make recvfromhvps\""))

    ;; 编译recverrmsg
    (add-to-list 'smart-compile-alist
                 '("recverrmsg\.cpp$" . "plink cnaps2@10.8.6.10 \". ~/.profile;cd app/ccms;make recverrmsg\""))

    ;; 编译sendtoccms
    (add-to-list 'smart-compile-alist
                 '("sendccms.+\.cpp$" . "plink cnaps2@10.8.6.10 \". ~/.profile;cd app/ccms;make sendtoccms\""))
    (add-to-list 'smart-compile-alist
                 '("sendccms.+\.h$" . "plink cnaps2@10.8.6.10 \". ~/.profile;cd app/ccms;make sendtoccms\""))

    ;; 编译recvfromccms
    (add-to-list 'smart-compile-alist
                 '("recvccms.+\.cpp$" . "plink cnaps2@10.8.6.10 \". ~/.profile;cd app/ccms;make recvfromccms\""))
    (add-to-list 'smart-compile-alist
                 '("recvccms.+\.h$" . "plink cnaps2@10.8.6.10 \". ~/.profile;cd app/ccms;make recvfromccms\""))

    ;; 编译sendtosaps
    (add-to-list 'smart-compile-alist
                 '("sendsaps.+\.cpp$" . "plink cnaps2@10.8.6.10 \". ~/.profile;cd app/saps;make sendtosaps\""))
    (add-to-list 'smart-compile-alist
                 '("sendsaps.+\.h$" . "plink cnaps2@10.8.6.10 \". ~/.profile;cd app/saps;make sendtosaps\""))

    (add-to-list 'smart-compile-alist
                 '("sendnets.+\.cpp$" . "plink cnaps2@10.8.6.10 \". ~/.profile;cd app/saps;make sendtosaps\""))
    (add-to-list 'smart-compile-alist
                 '("sendnets.+\.h$" . "plink cnaps2@10.8.6.10 \". ~/.profile;cd app/saps;make sendtosaps\""))
    ;; 编译recvfromsaps
    (add-to-list 'smart-compile-alist
                 '("recvsaps.+\.cpp$" . "plink cnaps2@10.8.6.10 \". ~/.profile;cd app/saps;make recvfromsaps\""))
    (add-to-list 'smart-compile-alist
                 '("recvsaps.+\.h$" . "plink cnaps2@10.8.6.10 \". ~/.profile;cd app/saps;make recvfromsaps\""))

    (add-to-list 'smart-compile-alist
                 '("recvnets.+\.cpp$" . "plink cnaps2@10.8.6.10 \". ~/.profile;cd app/saps;make recvfromsaps\""))
    (add-to-list 'smart-compile-alist
                 '("recvnets.+\.h$" . "plink cnaps2@10.8.6.10 \". ~/.profile;cd app/saps;make recvfromsaps\""))
    ;; 编译sendtomb
    (add-to-list 'smart-compile-alist
                 '("sendmb.+\.cpp$" . "plink cnaps2@10.8.6.10 \". ~/.profile;cd app/mb;make sendtomb\""))
    (add-to-list 'smart-compile-alist
                 '("sendmb.+\.h$" . "plink cnaps2@10.8.6.10 \". ~/.profile;cd app/mb;make sendtomb\""))

    ;; 编译recvfrommb
    (add-to-list 'smart-compile-alist
                 '("recvmb.+\.cpp$" . "plink cnaps2@10.8.6.10 \". ~/.profile;cd app/mb;make recvfrommb;make recvfromclt\""))
    (add-to-list 'smart-compile-alist
                 '("recvmb.+\.h$" . "plink cnaps2@10.8.6.10 \". ~/.profile;cd app/mb;make recvfrommb;make recvfromclt\""))


    ;; 编译mbcharge
    (add-to-list 'smart-compile-alist
                 '("mbcharge\.cpp$" . "plink cnaps2@10.8.6.10 \". ~/.profile;cd app/mb;make\""))
    (add-to-list 'smart-compile-alist
                 '("mbcharge\.h$" . "plink cnaps2@10.8.6.10 \". ~/.profile;cd app/mb;make\""))
    ))

(defun my-program/init-elog ()
  (use-package elog))

(defun my-program/init-code-library ()
  (use-package code-library))
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
