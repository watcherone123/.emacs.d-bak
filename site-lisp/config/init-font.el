;;; init-accelerate.el --- Font configuration. -*- lexical-binding: t -*-

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
;; Font configuration
;;

;;; Code:
(require 'init-funcs)

(defcustom sky-font '("Fira Code" . 11)
  "Set font."
  :group 'centaur
  :type '(alist :key-type string :value-type number))
;; Font
(defvar font-list '(("Input" . 11)
                    ("Fira Code" . 11)
                    ("Source Code Pro" . 12)
                    ("Ubuntu Mono" . 13)
                    ("Consolas" . 12)
                    ("DejaVu Sans Font" . 11)
                    ("SauceCodePro Nerd Font Mono" . 11)
                    ("monofur" . 11)
                    ("Anonymous Pro" . 11))
  "List of fonts and sizes.  The first one available will be used.")

;; FontFun
(defun change-font ()
  "Documentation."
  (interactive)
  (let* (available-fonts font-name font-size font-setting)
    (dolist (font font-list (setq available-fonts (nreverse available-fonts)))
      (when (member (car font) (font-family-list))
        (push font available-fonts)))
    (if (not available-fonts)
        (message "No fonts from the chosen set are available")
      (if (called-interactively-p 'interactive)
          (let* ((chosen (assoc-string (completing-read "What font to use? " available-fonts nil t) available-fonts)))
            (setq font-name (car chosen) font-size (read-number "Font size: " (cdr chosen))))
        (setq font-name (caar available-fonts) font-size (cdar available-fonts)))
      (setq font-setting (format "%s-%d" font-name font-size))
      (set-frame-font font-setting nil t)
      (add-to-list 'default-frame-alist (cons 'font font-setting))
      (setq-local font-name-s (format "\"%s\"" font-name))
      (sky-set-variable 'sky-font (cons font-name-s font-size)))))

;; Fonts
(defun sky-set-fonts ()
    "Set default font."
    (if (font-installed-p (car sky-font))
        (set-face-attribute 'default nil
                                        :font (car sky-font)
                                        :height (* (cdr sky-font) 10))
      (progn (message "can't find font %s" (car sky-font))
             (cl-loop for (font . size) in font-list
                      when (font-installed-p font)
                      return (set-face-attribute 'default nil
                                                  :font font
                                                  :height (* size 10)))))
    ;; Specify font for all unicode characters
    (set-fontset-font t 'unicode "Symbola" nil 'prepend)
    ;; Specify font for Chinese characters
    (cl-loop for font in '("WenQuanYi Micro Hei" "Microsoft Yahei")
            when (font-installed-p font)
            return (set-fontset-font t '(#x4e00 . #x9fff) font)))

(add-hook 'after-init-hook #'(lambda ()
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (select-frame frame)
                  (sky-set-fonts)))
    (when (display-graphic-p)
      (sky-set-fonts)))
) 
)


(defun +sky/toggle-big-font ()
  "切换大字体模式"
  (interactive)
  (if (> +sky/font-size 17.5)
      (setq +sky/font-size (- +sky/font-size 5))
    (setq +sky/font-size (+ +sky/font-size 5)))
  (+sky/set-fonts)
  (+sky/set-cn-fonts))

(provide 'init-font)
