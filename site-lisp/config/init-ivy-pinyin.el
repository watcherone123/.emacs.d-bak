;;; init-ivy-pinyin.el --- Init ivy pinyin config -*- lexical-binding: t -*-

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
;; Init ivy  pinyin config
;;

;;; Code:

(require 'pinyinlib)

(defun re-builder-pinyin (str)
  (or (pinyin-to-utf8 str)
      (ivy--regex-plus str)
      (ivy--regex-ignore-order)
      ))

(setq ivy-re-builders-alist
      '(
        (t . re-builder-pinyin)
        ))

(defun my-pinyinlib-build-regexp-string (str)
  (progn
    (cond ((equal str ".*")
           ".*")
          (t
           (pinyinlib-build-regexp-string str t))))
  )
(defun my-pinyin-regexp-helper (str)
  (cond ((equal str " ")
         ".*")
        ((equal str "")
         nil)
        (t
         str)))

(defun pinyin-to-utf8 (str)
  (cond ((equal 0 (length str))
         nil)
        ((equal (substring str 0 1) "!")
         (mapconcat 'my-pinyinlib-build-regexp-string
                    (remove nil (mapcar 'my-pinyin-regexp-helper (split-string
                                                                  (replace-regexp-in-string "!" "" str) "")))
                    ""))
        nil))
(provide 'init-ivy-pinyin)