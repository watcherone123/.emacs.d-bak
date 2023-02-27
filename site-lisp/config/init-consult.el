;;; init-consult.el --- Init consult config -*- lexical-binding: t -*-

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
;; Init consult config
;;

;;; Code:

(require 'consult)
(require 'consult-imenu)

(setq register-preview-delay 0.1)
(setq register-preview-function #'consult-register-format)
(setq xref-show-xrefs-function #'consult-xref)
(setq xref-show-definitions-function #'consult-xref)

(defcustom my-consult-ripgrep-or-line-limit 1000
  "Buffer size threshold for `my-consult-ripgrep-or-line'.
When the number of characters in a buffer exceeds this threshold,
`consult-ripgrep' will be used instead of `consult-line'."
  :type 'integer)

  (defun consult-ripgrep-one-file ()
  "Call `consult-ripgrep' for the current buffer (a single file)."
  (interactive)
  (let ((consult-project-root-function (lambda nil nil))
        (consult-ripgrep-args
         (concat "rg "
                 "--null "
                 "--line-buffered "
                 "--color=never "
                 "--line-number "
                 "--smart-case "
                 "--no-heading "
                 "--max-columns=1000 "
                 "--max-columns-preview "
                 "--search-zip "
                 "--with-filename "
                 (shell-quote-argument buffer-file-name))))
    (consult-ripgrep)))
	
(unless recentf-mode
	(recentf-mode 1))

(provide 'init-consult)