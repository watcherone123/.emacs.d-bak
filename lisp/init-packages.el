;;; init-package.el --- Initialize package configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Always load the newer .el or .elc file.
;; (setq load-prefer-newer t)
;;
;; ELPA: refer to https://github.com/melpa/melpa and https://elpa.emacs-china.org/.
;;

;; (require 'package)
(require 'init-constants)

;; Proxy
;; (setq url-proxy-services
;;       '(("http" . "127.0.0.1:7890")
;;         ("https" . "127.0.0.1:7890")))

;; Initialize packages
;; (unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
;;   (setq package-enable-at-startup nil)          ; To prevent initializing twice
;;   (package-initialize))

;; (defcustom shadow-package-archives 'tuna
;;   "Set package archives from which to fetch."
;;   :group 'shadow
;;   :type '(choice
;;           (const :tag "Melpa" melpa)
;;           (const :tag "Melpa Mirror" melpa-mirror)
;;           (const :tag "Emacs-China" emacs-china)
;;           (const :tag "Netease" netease)
;;           (const :tag "Tuna" tuna)))

;; (defun set-package-archives (archives)
;;   "Set specific package ARCHIVES repository."
;;   (interactive
;;    (list (intern (completing-read "Choose package archives: "
;;                                   '(melpa melpa-mirror emacs-china netease tuna)))))

;;   (setq package-archives
;;         (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
;;                             (not (gnutls-available-p))))
;;                (proto (if no-ssl "http" "https")))
;;           (pcase archives
;;             ('melpa
;;              `(,(cons "gnu"    (concat proto "://elpa.gnu.org/packages/"))
;;                ,(cons "melpa"  (concat proto "://melpa.org/packages/"))
;;                ,(cons "nongnu" (concat proto "://elpa.nongnu.org/nongnu/"))))
;;             ('melpa-mirror
;;              `(,(cons "gnu"    (concat proto "://elpa.gnu.org/packages/"))
;;                ,(cons "melpa"  (concat proto "://www.mirrorservice.org/sites/melpa.org/packages/"))))
;;             ('emacs-china
;;              `(,(cons "gnu"    (concat proto "://1.15.88.122/gnu/"))
;;                ,(cons "melpa"  (concat proto "://1.15.88.122/melpa/"))
;;                ,(cons "nongnu" (concat proto "://1.15.88.122/nongnu/"))))
;;             ('netease
;;              `(,(cons "gnu"    (concat proto "://mirrors.163.com/elpa/gnu/"))
;;                ,(cons "melpa"  (concat proto "://mirrors.163.com/elpa/melpa/"))
;;                ,(cons "nongnu" (concat proto "://mirrors.163.com/nongnu/"))))
;;             ('tuna
;;              `(,(cons "gnu"    (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
;;                ,(cons "melpa"  (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))
;;                ,(cons "nongnu" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/"))))
;;             (archives
;;              (error "Unknown archives: '%s'" archives)))))

;;   (message "Set package archives to '%s'." archives))

;; (set-package-archives shadow-package-archives)

;; (unless emacs/>=29.1p
;;   (defun shadow-install-use-package ()
;;     "Install `use-package'."
;;     (unless (package-installed-p 'use-package)
;;       (message "%s" "Refreshing package database...")
;;       (package-refresh-contents)
;;       (message "done.")
;;       (package-install 'use-package)))

;;   (shadow-install-use-package))

;; (eval-when-compile
;;   (require 'use-package)

;;   ;; Should set before loading `use-package'
;;   (setq use-package-always-ensure t)
;;   ;; (setq use-package-always-defer t)
;;   (setq use-package-expand-minimally t)
;;   (setq use-package-enable-imenu-support t))

;; ;; Update GPG keyring for GNU ELPA
;; (use-package gnu-elpa-keyring-update
;;   :defer t)

;; ;; Auto update packages
;; (use-package auto-package-update
;;   :defer t
;;   :init
;;   (setq auto-package-update-delete-old-versions t
;; ;        auto-package-update-last-update-day-filename (concat shadow-cache-dir "/.last-package-update-day")
;;         auto-package-update-hide-results t)
;;   (defalias 'upgrade-packages #'auto-package-update-now))


;; Load `custom-file'
;; (and (file-readable-p custom-file) (load custom-file))

;; (eval-and-compile ; `borg'
;;   (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
;;   (require 'borg)
;;   (borg-initialize))

(eval-and-compile ; `use-package'
  (setq use-package-enable-imenu-support t)
  (setq use-package-expand-minimally t)
  (setq use-package-compute-statistics t)
  (require  'use-package))

(unless (fboundp 'restart-emacs)
  (use-package restart-emacs
      :defer t))

;; (use-package emacsql)
;; (use-package closql)

;; (when (>= emacs-major-version 29)
;;   (setq epkg-database-connector 'sqlite-builtin))
;; (use-package epkg)

(use-package esup
  ;; :disabled
  :defer t)

(provide 'init-packages)
;;; init-packages.el ends here
