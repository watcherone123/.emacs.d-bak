;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 watcherone123

;; Author: watcherone123 <watcherone123@gmail.com>
;; URL: https://github.com/watcherone123/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;

;;; Code:

(require 'cl-lib)

(setq gc-cons-threshold most-positive-fixnum)

;; Prevent unwanted runtime compilation for gccemacs (native-comp) users;
;; packages are compiled ahead-of-time when they are installed and site files
;; are compiled when gccemacs is installed.
(setq native-comp-deferred-compilation nil ;; obsolete since 29.1
      native-comp-jit-compilation nil)

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; no title bar
;; (add-to-list 'default-frame-alist '(undecorated . t))
;; Faster to disable these here (before they've been initialized)
(push '(alpha-background . 85) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; (push '(undecorated . t) default-frame-alist)
(setq initial-major-mode 'fundamental-mode ;; 默认用最简单的模式
      package--init-file-ensured t
      inhibit-startup-message t             ; 关闭启动 Emacs 时的欢迎界面
      )

(defun sky-add-subdirs-to-load-path (lisp-dir)
  "Add sub-directories under LISP-DIR into `load-path'."
  (let* ((default-directory lisp-dir))
    (setq load-path
          (append
           (delq nil
                 (mapcar (lambda (dir)
                           (unless (string-match "^\\." dir)
                             (expand-file-name dir)))
                         (directory-files lisp-dir)))
           load-path))))

    (sky-add-subdirs-to-load-path (file-name-as-directory "~/.emacs.d/site-lisp/pkg"))
  (add-to-list 'load-path "~/.emacs.d/site-lisp/config")


(require 'init)