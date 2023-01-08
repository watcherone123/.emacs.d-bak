
;;; init-org-capture.el  --- init-org-capture setup. -*- lexical-binding: t -*-

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
;; init-org-capture setup
;;

;;; Code:

(require 'org-capture)

(defun +sky/setup-org-capture ()
  (setq org-capture-templates nil)
  (push '("j" "我的日志" entry (file+headline"~/Docs/org/diary.org" "日志") "* %U - %^{标题}\n  %?") org-capture-templates)
  (push '("i" "我的闪念" entry (file+headline "~/Docs/org/idea.org" "闪念") "* %U - %^{标题} %^g\n  %?\n") org-capture-templates)
  (push '("k" "我的百科" entry (file+headline "~/Docs/org/wiki.org" "WIKI") "* %^{标题} %t %^g\n  %?\n") org-capture-templates)
  (push '("t" "任务" entry (file+headline "~/Docs/org/todo.org" "任务") "* TODO %^{标题} %t %^g\n  %?\n") org-capture-templates)
  (push '("n" "LNKS" entry (file+headline "~/Docs/org/lnks.org" "任务") "* TODO %^{标题} %t\n  %?\n") org-capture-templates))

(provide 'init-org-capture)