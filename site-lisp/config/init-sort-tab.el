
;;; init-sort_tab.el  --- init-sort_tab setup. -*- lexical-binding: t -*-

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
;; init-sort_tab setup
;;

;;; Code:

(require 'sort-tab)

(sort-tab-mode t)

(one-key-create-menu
 "sort-tab"
 '((("h" . "Switch to previous tab") . sort-tab-select-prev-tab)
   (("l" . "Switch to next tab") . sort-tab-select-next-tab)
   (("L" . "Switch to last tab") . sort-tab-select-last-tab)
   (("H" . "Switch to first tab") . sort-tab-select-first-tab)
   (("d" . "Close current tab") . sort-tab-close-current-tab)
   (("a" . "Close all tab") . sort-tab-close-all-tabs)
   ))


(set-face-attribute 'sort-tab-current-tab-face nil
		    :background (face-background 'highlight)
		    :foreground (face-attribute 'highlight :foreground))

(set-face-attribute 'sort-tab-separator-face nil
		    :background nil
		    :foreground (face-attribute 'font-lock-comment-face :foreground))

(set-face-attribute 'sort-tab-other-tab-face nil
		    :background (face-background 'default )
		    :foreground (face-foreground 'default))

(provide 'init-sort-tab)
;;; init-sort-tab.el ends here
