;;; init-accelerate.el --- Font configuration. -*- lexical-binding: t -*-

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
;; Font configuration
;;

;;; Code:


(defun load-font-setup()
  (cond ((eq window-system 'pgtk)
         (set-face-attribute 'default nil :height 140 :family "WenQuanYi Micro Hei Mono"))
        (t
         (let ((emacs-font-size 14)
               (chinese-font-name  "TsangerJinKai03-6763")
               english-font-name)
           (cond
            ((featurep 'cocoa)
             (setq english-font-name "Monaco"))
            ((string-equal system-type "gnu/linux")
             (setq english-font-name "WenQuanYi Micro Hei Mono")))
           (when (display-grayscale-p)
             (set-frame-font (format "%s-%s" (eval english-font-name) (eval emacs-font-size)))
             (set-fontset-font (frame-parameter nil 'font) 'unicode (eval english-font-name))

             (dolist (charset '(kana han symbol cjk-misc bopomofo))
               (set-fontset-font (frame-parameter nil 'font) charset (font-spec :family (eval chinese-font-name))))
             )))))

(load-font-setup)

(dolist (hook (list
               'c-mode-common-hook
               'c-mode-hook
               'c++-mode-hook
               'java-mode-hook
               'haskell-mode-hook
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'lisp-mode-hook
               'maxima-mode-hook
               'ielm-mode-hook
               'sh-mode-hook
               'makefile-gmake-mode-hook
               'php-mode-hook
               'python-mode-hook
               'js-mode-hook
               'go-mode-hook
               'qml-mode-hook
               'jade-mode-hook
               'css-mode-hook
               'ruby-mode-hook
               'coffee-mode-hook
               'rust-mode-hook
               'qmake-mode-hook
               'lua-mode-hook
               'swift-mode-hook
               'web-mode-hook
               'markdown-mode-hook
               'llvm-mode-hook
               'conf-toml-mode-hook
               'nim-mode-hook
               'typescript-mode-hook
               ))
  (add-hook hook #'(lambda () (load-font-setup))))

(defun +sky/toggle-big-font ()
  "切换大字体模式"
  (interactive)
  (if (> +sky/font-size 17.5)
      (setq +sky/font-size (- +sky/font-size 5))
    (setq +sky/font-size (+ +sky/font-size 5)))
  (+sky/set-fonts)
  (+sky/set-cn-fonts))

(provide 'init-font)
