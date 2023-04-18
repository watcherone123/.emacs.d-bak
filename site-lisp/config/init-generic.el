;;; init.el --- Generic config. -*- lexical-binding: t -*-

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
;; Generic config
;;

;;; Code:

;; Restore emacs session.
;; (setq initial-buffer-choice t)
;; (run-with-timer 1 nil #'(lambda () (bury-buffer)))

;; 增加长行处理性能
(setq bidi-inhibit-bpa t)
(setq-default bidi-paragraph-direction 'left-to-right)

;; 增加IO性能
(setq process-adaptive-read-buffering nil)
(setq read-process-output-max (* 1024 1024))

(fset 'yes-or-no-p 'y-or-n-p)           ;以 y/n代表 yes/no
(setq use-dialog-box nil)               ;never pop dialog
(setq inhibit-startup-screen t)         ;inhibit start screen
(setq use-file-dialog nil)              ;禁用 Emacs 使用系统对话框
(setq initial-scratch-message nil)      ;关闭启动空白buffer, 这个buffer会干扰session恢复
(setq-default comment-style 'indent)    ;设定自动缩进的注释风格
(setq ring-bell-function 'ignore)       ;关闭烦人的出错时的提示声
(setq default-major-mode 'text-mode)    ;设置默认地主模式为TEXT模式
(setq mouse-yank-at-point t)            ;粘贴于光标处,而不是鼠标指针处
(setq x-select-enable-clipboard t)      ;支持emacs和外部程序的粘贴
(setq inhibit-compacting-font-caches t) ;使用字体缓存，避免卡顿
(setq confirm-kill-processes nil)       ;退出自动杀掉进程
(setq async-bytecomp-allowed-packages nil) ;避免magit报错
(setq word-wrap-by-category t)             ;按照中文折行
(add-hook 'find-file-hook 'highlight-parentheses-mode t) ;增强的括号高亮

(run-with-timer 1 nil #'(lambda () (save-place-mode t)))

(setq ad-redefinition-action 'accept)   ;不要烦人的 redefine warning
(setq frame-resize-pixelwise t) ;设置缩放的模式,避免Mac平台最大化窗口以后右边和下边有空隙
;; 平滑地进行半屏滚动，避免滚动后recenter操作
(setq scroll-step 1
      scroll-conservatively 10000)

;; 不显示 *scratch*
(defun remove-scratch-buffer ()
 (if (get-buffer "*scratch*")
     (kill-buffer "*scratch*")))
(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; Don't ask me when close emacs with process is running
(require 'cl-lib)
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-letf (((symbol-function #'process-list) (lambda ())))
    ad-do-it))

;; Don't ask me when kill process buffer
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))
(provide 'init-generic)
