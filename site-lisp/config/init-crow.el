;;; init-crow.el --- crow configuration. -*- lexical-binding: t -*-

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
;; crow configuration
;;

;;; Code:

(setq
   ;; crow开启的翻译信息
   crow-enable-info '(:examples nil
                      :source nil
                      :translit nil
                      :translation t
                      :options nil)
   ;; crow翻译间隔延迟
   crow-translate-delay 0.2
   ;; crow翻译单位类型
   crow-translate-type (list 'word 'sentence)
   ;; 翻译文本ui呈现类型
   crow-ui-type '(posframe eldoc)
   ;; posframe超时隐藏时间
   crow-posframe-hide-timeout 3
   ;; crow posframe放置的位置
   ;; crow-posframe-position 'point
   )

(require 'crow)

(one-key-create-menu
 "Crow"
 '((("u" . "Next crow ui type") . crow-next-ui-type)
   (("t" . "Next crow translate type") . crow-next-translate-type)))

(provide 'init-crow)