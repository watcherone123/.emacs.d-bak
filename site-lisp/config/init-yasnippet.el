
;;; init-yasnippet.el --- Yasnippet configuration -*- lexical-binding: t -*-

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
;; Yasnippet configuration
;;

;;; Code:

;;; Require
(add-hook 'prog-mode-hook
          #'(lambda ()
              (require 'yasnippet)

              (defun get-git-user-name ()
                (interactive)
                (replace-regexp-in-string "\n$" "" (shell-command-to-string "git config --get user.name")))

              (defun get-git-user-email ()
                (interactive)
                (replace-regexp-in-string "\n$" "" (shell-command-to-string "git config --get user.email")))

              (add-to-list `yas/root-directory (concat sky-emacs-root-dir "/snippets"))
              (yas-global-mode 1)

              ;; Disable yasnippet mode on some mode.
              (dolist (hook (list
                             'term-mode-hook
                             ))
                (add-hook hook #'(lambda () (yas-minor-mode -1))))
              ))

(provide 'init-yasnippet)