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

;;; Code:

(require 'init-gcmh)

;; 加速配置
(require 'init-accelerate)

;; 字体设置
(require 'init-font)

(let (
      ;; 清空避免加载远程文件的时候分析文件。
      (file-name-handler-alist nil))

    ;; 定义一些启动目录，方便下次迁移修改
    (defvar sky-emacs-root-dir (file-truename "~/.emacs.d/site-lisp"))
    (defvar sky-emacs-config-dir (concat sky-emacs-root-dir "/config"))
    (defvar sky-emacs-extension-dir (concat sky-emacs-root-dir "/pkg"))

  (with-temp-message ""              ;抹掉插件启动的输出

    ;;(require 'init-fullscreen)

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
    (require 'init-rime)
   
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
         (require 'init-ivy) 
         (require 'init-lsp-bridge)
         (require 'init-meow)
         (require 'init-key)

         (require 'init-tree-sitter)
         (require 'init-eldoc)
         (require 'init-yasnippet)
         (require 'init-cursor-chg)
         (require 'init-winpoint)
         (require 'init-info)
         (require 'init-org)
         (require 'init-idle)
         (require 'init-markdown-mode)
         (require 'init-olivetti)

         (require 'init-eaf)
        ;;  (require 'init-popweb)

         (require 'init-sort-tab)
         ))))

(provide 'init)