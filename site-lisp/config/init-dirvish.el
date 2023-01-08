
;;; init-dirvish.el --- Config for dirvish-*- lexical-binding: t -*-

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
;; Config for dirvish
;;

(require 'dirvish)
(require 'dirvish-side)
(dirvish-override-dired-mode)
(lazy-load-set-keys '(("h" . dired-up-directory)
		      ("l" . dired-find-file))
		    dirvish-mode-map)

(add-hook 'dirvish-mode-hook (lambda ()
			       (+sky/meow-add-motion-mode-alist 'dired-mode)
			       (meow-define-keys
				   'motion
				 '("h" . dired-up-directory)
				 '("l" . dired-find-file)
				 '("x" . dired-do-flagged-delete)
				 )))

(defun +sky/dirvish-side-current-path ()
  (interactive)
  (dirvish-side (buffer-file-name)))

;; FACE
;; (set-face-background 'dirvish-hl-line (face-background 'default))

(provide 'init-dirvish)