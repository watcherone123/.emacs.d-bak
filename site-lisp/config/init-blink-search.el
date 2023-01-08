
;;; init-blink-search.el --- Config for blink search-*- lexical-binding: t -*-

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
;; Config for blink search
;;

;;; Require
(require 'blink-search)

;;; Code:

(setq blink-search-common-directory '(("HOME" "~/") 
                                      ("CONFIG" "~/.emacs.d/site-lisp/config/")
                                      ("REPO" "~/.emacs.d/pkg")
                                      ))

(setq blink-search-enable-posframe nil)
(setq blink-search-enable-log nil)
(add-hook 'blink-search-mode-hook #'meow-insert-mode)

(provide 'init-blink-search)

;;; init-blink-search.el ends here
