;;; init-tree-sitter.el  --- Configure for tree sitter. -*- lexical-binding: t -*-

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
;; Configure for tree sitter
;;

;;; Code:

;;; Require
(require 'tree-sitter)
(require 'tree-sitter-hl)
(require 'tree-sitter-langs)
(require 'tree-sitter-debug)
(require 'tree-sitter-query)

;;; Code:
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;; Add Emacs-Lisp for tree-sitter:
;;
;; 1. git clone https://github.com/Wilfred/tree-sitter-elisp
;; 2. gcc ./src/parser.c -fPIC -I./ --shared -o elisp.so
;; 3. cp ./elisp.so ~/.tree-sitter-langs/bin (~/.tree-sitter-langs/bin is path of your tree-sitter-langs repo)
;; (tree-sitter-load 'elisp)
;; (add-to-list 'tree-sitter-major-mode-language-alist '(emacs-lisp-mode . elisp))
;; (add-to-list 'tree-sitter-major-mode-language-alist '(inferior-emacs-lisp-mode . elisp))

;; Add Vue for tree-sitter:
;;
;; 1. git clone https://github.com/ikatyang/tree-sitter-vue.git
;; 2. gcc ./src/parser.c ./src/scanner.cc -fPIC -I./ --shared -o vue.so
;; 3. cp ./vue.so ~/.tree-sitter-langs/bin (~/.tree-sitter-langs/bin is path of your tree-sitter-langs repo)
;; (tree-sitter-load 'vue)
;; (add-to-list 'tree-sitter-major-mode-language-alist '(web-mode . vue))

;; Add Typescript for tree-sitter.
;;
;; 1. git clone https://github.com/tree-sitter/tree-sitter-typescript.git
;; 2. gcc ./tsx/src/parser.c ./tsx/src/scanner.cc -fPIC -I./ --shared -o typescript.so
;; 3. cp ./typescript.so ~/.tree-sitter-langs/bin (~/.tree-sitter-langs/bin is path of your tree-sitter-langs repo)
;; (tree-sitter-load 'typescript)
;; (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-mode . typescript))

(provide 'init-tree-sitter)

;;; init-tree-sitter.el ends here