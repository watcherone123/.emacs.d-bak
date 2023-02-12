;;; init.el --- initialization. -*- lexical-binding: t -*-

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
(require 'init-gcmh)
;;; Code:
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6) ;;defer gc futher back

;; 字体设置
;; (require 'init-font)
(let (
      ;; 清空避免加载远程文件的时候分析文件。
      (file-name-handler-alist nil))

  ;; 定义一些启动目录，方便下次迁移修改
  (defvar sky-emacs-root-dir (file-truename "~/.emacs.d/site-lisp"))
  (defvar sky-emacs-config-dir (concat sky-emacs-root-dir "/config"))
  (defvar sky-emacs-extension-dir (concat sky-emacs-root-dir "/pkg"))

  (with-temp-message ""              ;抹掉插件启动的输出

    (require 'init-fullscreen)
    (require 'init-generic)
    (require 'lazycat-theme)
    ;; (lazycat-theme-load-with-sunrise)
    (lazycat-theme-load-dark)
    (require 'lazy-load)
    (require 'one-key)
    (require 'grammatical-edit)
    (require 'display-line-numbers)
    (require 'basic-toolkit) ;;TODO: mabe remove
    (require 'redo) ;;TODO: mabe remove
    (require 'init-emacs)

    (require 'init-highlight-parentheses)
    (require 'init-awesome-tray)
    (require 'init-line-number)
    (require 'init-auto-save)
    (require 'init-mode)
    (require 'init-grammatical-edit)
    (require 'init-one-key)
    ;; (require 'init-rime)
    ;; 可以延后加载的
    (run-with-idle-timer
     1 nil
     #'(lambda ()
         (require 'pretty-lambdada)
         (require 'browse-kill-ring)
         (require 'elf-mode)
         (require 'init-indent)
         (require 'init-window)
         (require 'init-dired)
         (require 'init-iedit)
         (require 'init-vertico) 
         (require 'init-orderless) 
         (require 'init-lsp-bridge)
         (require 'init-meow)
         (require 'init-key)

        ;;  (require 'init-tree-sitter)
         (require 'init-eldoc)
         (require 'init-yasnippet)
         (require 'init-cursor-chg)
         (require 'init-winpoint)
         (require 'init-info)
         (require 'init-org)
         (require 'init-idle)
         (require 'init-markdown-mode)
         (require 'init-olivetti)
         (require 'init-editor)
         (require 'init-symbol-overlay)

         (require 'init-eaf)
        ;;  ;;  (require 'init-popweb)
        ;;  ;; Restore session at last.
         (require 'init-session)
         (emacs-session-restore)
         (require 'init-sort-tab)

         ))))


;; @see https://www.reddit.com/r/emacs/comments/55ork0/is_emacs_251_noticeably_slower_than_245_on_windows/
;; Emacs 25 does gc too frequently
;; (setq garbage-collection-messages t) ; for debug
;; (defun my-cleanup-gc ()
;;   "Clean up gc."
;;   (setq gc-cons-threshold  67108864) ; 64M
;;   (setq gc-cons-percentage 0.1) ; original value
;;   (garbage-collect))

;; (run-with-idle-timer 4 nil #'my-cleanup-gc)

(message "*** Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time (time-subtract after-init-time before-init-time)))
           gcs-done)

(provide 'init)
