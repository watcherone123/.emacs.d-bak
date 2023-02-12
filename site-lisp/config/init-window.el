;;; init-window.el  --- Configure for window. -*- lexical-binding: t -*-

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
;; Configure for window
;;
(require 'switch-window)
(require 'zoom)
;;; Code:

(custom-set-variables
 '(zoom-mode t))
(defun size-callback ()
  (cond ((> (frame-pixel-width) 1280) '(90 . 0.75))
        (t                            '(0.5 . 0.5))))
(custom-set-variables
 '(zoom-size 'size-callback))
(custom-set-variables
 '(zoom-ignored-major-modes '(dired-mode markdown-mode))
 '(zoom-ignored-buffer-names '("zoom.el" "init.el"))
 '(zoom-ignored-buffer-name-regexps '("^*calc"))
 '(zoom-ignore-predicates '((lambda () (> (count-lines (point-min) (point-max)) 20)))))

;; 窗口移动
(setq x-underline-at-descent-line t)

;; 绘制窗口线
(setq window-divider-default-places t
      window-divider-default-right-width 2
      window-divider-default-bottom-width 2)

(window-divider-mode t)
(winner-mode t)
(one-key-create-menu
 "Window"
 '((("o" . "swicth window") . switch-window)
   (("l" . "Focus right window") . windmove-right)
   (("h" . "Focus left window") . windmove-left)
   (("k" . "Focus up window") . windmove-up)
   (("j" . "Focus down window") . windmove-down)
   (("L" . "Swap right window") . windmove-swap-states-right)
   (("H" . "Swap left window") . windmove-swap-states-left)
   (("K" . "Swap up window") . windmove-swap-states-up)
   (("J" . "Swap down window") . windmove-swap-states-down)
   (("s" . "Split window vertically") . split-window-below)
   (("v" . "Split window horizontally") . split-window-right)
   (("d" . "Delete window") . delete-window)
   (("u" . "Undo window") . winner-undo)
   (("C-h" . "Resize window to smaller") . shrink-window-horizontally)
   (("m" . "Delete other window") . delete-other-windows)
   (("C-k" . "Scroll other window up") . scroll-other-window-down)
   (("C-j" . "Scroll other window down") . scroll-other-window)))
   
(provide 'init-window)
