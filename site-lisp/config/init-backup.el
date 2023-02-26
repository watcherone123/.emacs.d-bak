;;; init-backup.el --- Init backup config -*- lexical-binding: t -*-

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
;; Init backup config
;;

;;; Code:
(require 'no-littering)
;; (require 'recentf)
;; (require 'savehist)

(setq recentf-max-saved-items 1000)
(setq recentf-exclude `("/tmp/" "/ssh:" ,(concat user-emacs-directory "lib/.*-autoloads\\.el\\'")))
(add-to-list 'recentf-exclude no-littering-var-directory)
(add-to-list 'recentf-exclude no-littering-etc-directory)

(setq history-length 10000)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history t)

(setq global-auto-revert-non-file-buffers t)

(run-with-timer 1 nil #'(lambda () 
    (recentf-mode t)
    (savehist-mode t)
    (save-place-mode t)
    (global-auto-revert-mode t)
))

(provide 'init-backup)