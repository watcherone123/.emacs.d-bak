
;;; init-editor.el --- init-editor config -*- lexical-binding: t -*-

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
;; init-editor config
;;

;;; Code:

(require 'hl-todo)

 (setq hl-todo-keyword-faces
	'(("TODO"   . "#FFFF00")
	  ("FIXME"  . "#FF3300")
	  ("DEBUG"  . "#FF3300")
	  ("GOTCHA" . "#FF3300")
	  ("STUB"   . "#FF3300")))

(global-hl-todo-mode t)

(add-hook 'prog-mode-hook (lambda ()
                            (prettify-symbols-mode)
							 (setq-default prettify-symbols-alist
                '(("lambda" . ?λ)
                  ("<-" . ?←)
                  ("->" . ?→)
                  ("->>" . ?↠)
                  ("=>" . ?⇒)
                  ("/=" . ?≠)
                  ("!=" . ?≠)
                  ("<=" . ?≤)
                  (">=" . ?≥)
                  ("=<<" . (?= (Br . Bl) ?≪))
                  (">>=" . (?≫ (Br . Bl) ?=))
                  ("<=<" . ?↢)
                  (">=>" . ?↣)))
				   (setq prettify-symbols-unprettify-at-point 'right-edge)
                            ))
  (add-hook 'asm-mode-hook
    (lambda ()
      ;; Preferred comment style
      (setq comment-start "// "
            comment-end "")))
(provide 'init-editor)
