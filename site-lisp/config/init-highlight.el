;;; init.el --- Config for highlight-parentheses-mode. -*- lexical-binding: t -*-

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
;; Config for highlight-parentheses-mode
;;

;;; Code:


;;; Require


(add-hook 'prog-mode-hook (lambda ()
                            (require 'highlight-parentheses)
                            (setq highlight-parentheses-delay 0.2)
                            (setq highlight-parentheses-colors '("Springgreen3"
                                                                 "IndianRed3"
                                                                 "IndianRed4"
                                                                 "yellow"
                                                                 ))))
(dolist (hook (list
               'prog-mode-hook
               'text-mode-hook
               'conf-mode-hook
               ))
  (add-hook hook #'(lambda ()
                     (require 'goggles)
                     (setq-default goggles-pulse nil)
                     (goggles-mode)
                     )))

(provide 'init-highlight)
