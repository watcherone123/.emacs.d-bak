
;;; init-eaf.el --- init-eaf config -*- lexical-binding: t -*-

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
;; init-eaf config
;;

;;; Code:

(require 'eaf)
;; (require 'eaf-video-player)
(require 'eaf-image-viewer)
;; (require 'eaf-file-browser)
(require 'eaf-browser)
(require 'eaf-pdf-viewer)
(require 'eaf-mindmap)
;; (require 'eaf-git)
;; (require 'eaf-file-sender)
;; (require 'eaf-terminal)
;; (require 'eaf-file-manager)

(defun eaf-translate-text (text)
  (interactive)
  (require 'init-crow)
  (crow--gen-translated-text "text")
  (message "%s" crow--translated-text))

(setq
 eaf-proxy-type "socks5"
 eaf-proxy-host "127.0.0.1"
 eaf-proxy-port "1088"
 eaf-browser-dark-mode nil)

(eaf-bind-key meow-keypad "SPC" eaf-browser-keybinding)
(eaf-bind-key eaf-py-proxy-insert_or_scroll_up_page "M-SPC" eaf-browser-keybinding)
(eaf-bind-key nil "u" eaf-browser-keybinding)

(provide 'init-eaf)

;;; init-eaf.el ends here
