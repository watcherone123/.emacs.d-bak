;;; init-org.el --- Configure for org-mode

;; Filename: init-org.el
;; Description: Configure for org-mode
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2020, Andy Stewart, all rights reserved.
;; Created: 2020-03-31 22:32:49
;; Version: 0.1
;; Last-Updated: 2020-03-31 22:32:49
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-org.el
;; Keywords:
;; Compatibility: GNU Emacs 28.0.50
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Configure for org-mode
;;

;;; Installation:
;;
;; Put init-org.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-org)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-org RET
;;

;;; Change log:
;;
;; 2020/03/31
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require


;;; Code:

(require 'org)
(require 'org-modern)
(require 'init-valign)
(require 'init-org-agenda)
(require 'init-org-capture)

(setq org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "KILL(k)")))

(let ((faces '(("TODO" . (:foreground "#fbf7f0" :background "#006800"))
	       ("WAIT" . (:foreground "#fbf7f0" :background "#f5d0a0"))
	       ("DONE" . (:foreground "black" :background "gray90"))
	       ("KILL" . (:foreground "#fbf7f0" :background "#a60000")))))
  (setq org-modern-todo-faces faces)
  (setq org-todo-keyword-faces faces))


(autoload '+sky/setup-org-capture "init-org-capture")
(autoload '+sky/setup-org-agenda "init-org-agenda")

(add-hook 'org-mode-hook
	  (lambda ()
	    (global-org-modern-mode +1)))

(+sky/setup-org-agenda)
(+sky/setup-org-capture)

(with-eval-after-load 'org
  (setq org-odt-preferred-output-format "docx") ;ODT转换格式默认为docx
  (setq org-startup-folded nil)                 ;默认展开内容
  (setq org-startup-indented t)                 ;默认缩进内容

  (defun org-export-docx ()
    (interactive)
    (let ((docx-file (concat (file-name-sans-extension (buffer-file-name)) ".docx"))
          (template-file (concat (file-name-as-directory lazycat-emacs-root-dir)
                                 (file-name-as-directory "template")
                                 "template.docx")))
      (shell-command (format "pandoc %s -o %s --reference-doc=%s"
                             (buffer-file-name)
                             docx-file
                             template-file
                             ))
      (message "Convert finish: %s" docx-file))))

(dolist (hook (list
               'org-mode-hook
               ))
  (add-hook hook #'(lambda ()
                     (require 'eaf)

                     (setq truncate-lines nil) ;默认换行

                     (lazy-load-set-keys
                      '(
                        ("M-h" . set-mark-command) ;选中激活
                        ("C-c C-o" . eaf-open-url-at-point)
                        )
                      org-mode-map
                      )
                     )))

(let ((emacs-font-size 14)
      (emacs-font-name "WenQuanYi Micro Hei Mono"))
  (set-frame-font (format "%s-%s" (eval emacs-font-name) (eval emacs-font-size)))
  (set-fontset-font (frame-parameter nil 'font) 'unicode (eval emacs-font-name)))

;; (with-eval-after-load 'org
;;   (defun org-buffer-face-mode-variable ()
;;     (interactive)
;;     (make-face 'width-font-face)
;;     (set-face-attribute 'width-font-face nil :font "等距更纱黑体 SC 15")
;;     (setq buffer-face-mode-face 'width-font-face)
;;     (buffer-face-mode))

;;   (add-hook 'org-mode-hook 'org-buffer-face-mode-variable))

(provide 'init-org)

;;; init-org.el ends here
