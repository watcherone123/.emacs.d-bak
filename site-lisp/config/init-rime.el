
;;; init-rime.el  --- init-rime setup. -*- lexical-binding: t -*-

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
;; init-rime setup
;;

;;; Code:

;;; Require
(require 'rime)

(setq default-input-method "rime")
(setq rime-user-data-dir (expand-file-name "rime" user-emacs-directory ))
(setq rime-share-data-dir (expand-file-name "rime" user-emacs-directory ))
(setq rime-show-candidate 'posframe)
(setq rime-posframe-properties
      (list :background-color "#2E3440"
            :foreground-color "#ECEFF4"
            ;; :font evan/en-font-name
            :internal-border-width 0))

(set-face-attribute 'rime-highlight-candidate-face nil :width 'normal :inherit 'highlight)

(provide 'init-rime)
