;;; init-ivy.el --- Init ivy config -*- lexical-binding: t -*-

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
;; Init ivy config
;;

;;; Code:

(require 'swiper)
(require 'ivy)
(require 'counsel)
(require 'amx)
(require 'init-ivy-pinyin)
(ivy-mode t)
(amx-mode t)				;

(require 'wgrep)
(setq-default ivy-use-virtual-buffers nil
	      ivy-height 25)


(add-to-list 'ivy-height-alist (cons 'counsel-switch-buffer 20))

(setq-default counsel-search-engine 'google)

(one-key-create-menu "Counsel"
		     '((("w" . "Switch desktop window.") . counsel-wmctrl)
		       (("s" . "Use search engine.") . counsel-search)
		       (("d" . "Run linux app") . counsel-linux-app)))

(provide 'init-ivy)