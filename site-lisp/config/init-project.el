
;;; init-project.el  --- init-project setup. -*- lexical-binding: t -*-

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
;; init-project setup
;;

;;; Code:

(require 'project)
(one-key-create-menu
 "Project"
 '(
   (("f" . "Find file in project") . project-find-file)
   (("a". "Remmeber a proejct") . project-remember-projects-under)
   (("p" . "Switch project") . project-switch-project)
   (("r" . "Remove known project") . project-forget-project)
   (("e" . "Project eshell") . project-eshell)))

(define-key project-prefix-map (kbd "b") #'blink-search)
(define-key project-prefix-map (kbd "m") #'magit)
(define-key project-prefix-map (kbd "d") #'dired)
(add-to-list 'project-switch-commands '(blink-search "BlinkSearch") t)
(add-to-list 'project-switch-commands '(magit "Magit") t)
(add-to-list 'project-switch-commands '(dired "Dired") t)


;;;###autoload
(defun +sky/project-recentf ()
  (interactive)
  (ivy-read "Recentf in project"
		       (-filter (lambda (it)
				  (s-contains? (car (-take-last 1 (project-current))) it)) recentf-list)
		       :action #'find-file))

(provide 'init-project)
