
;;; init-mode.el  --- File mode setup. -*- lexical-binding: t -*-

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
;; File mode setup
;;

;;; Code:

;;; ### auto-mode-alist ###
;;; --- 绑定扩展名到特定的模式
(defun add-to-alist (alist-var elt-cons &optional no-replace)
  "Add to the value of ALIST-VAR an element ELT-CONS if it isn't there yet.
If an element with the same car as the car of ELT-CONS is already present,
replace it with ELT-CONS unless NO-REPLACE is non-nil; if a matching
element is not already present, add ELT-CONS to the front of the alist.
The test for presence of the car of ELT-CONS is done with `equal'."
  (let ((existing-element (assoc (car elt-cons) (symbol-value alist-var))))
    (if existing-element
        (or no-replace
            (rplacd existing-element (cdr elt-cons)))
      (set alist-var (cons elt-cons (symbol-value alist-var)))))
  (symbol-value alist-var))

(dolist (elt-cons '(
                    ("\\.markdown" . markdown-mode)
                    ("\\.md" . markdown-mode)
                    ("\\.coffee$" . coffee-mode)
                    ("\\.iced$" . coffee-mode)
                    ("Cakefile" . coffee-mode)
                    ("\\.stumpwmrc\\'" . lisp-mode)
                    ("\\.[hg]s\\'" . haskell-mode)
                    ("\\.hi\\'" . haskell-mode)
                    ("\\.hs-boot\\'" . haskell-mode)
                    ("\\.chs\\'" . haskell-mode)
                    ("\\.l[hg]s\\'" . literate-haskell-mode)
                    ("\\.inc\\'" . asm-mode)
                    ("\\.max\\'" . maxima-mode)
                    ("\\.org\\'" . org-mode)
                    ("\\.cron\\(tab\\)?\\'" . crontab-mode)
                    ("cron\\(tab\\)?\\." . crontab-mode)
                    ("\\.a90\\'" . intel-hex-mode)
                    ("\\.hex\\'" . intel-hex-mode)
                    ("\\.py$" . python-mode)
                    ("SConstruct". python-mode)
                    ("\\.ml\\'" . tuareg-mode)
                    ("\\.mli\\'" . tuareg-mode)
                    ("\\.mly\\'" . tuareg-mode)
                    ("\\.mll\\'" . tuareg-mode)
                    ("\\.mlp\\'" . tuareg-mode)
                    ("\\.qml\\'" . qml-mode)
                    ("\\.jl\\'" . lisp-mode)
                    ("\\.asdf\\'" . lisp-mode)
                    ("CMakeLists\\.txt\\'" . cmake-mode)
                    ("\\.cmake\\'" . cmake-mode)
                    ("\\.php\\'" . php-mode)
                    ("\\.vue" . web-mode)
                    ("\\.wxml" . web-mode)
                    ("\\.blade\\.php\\'" . web-mode)
                    ("\\.phtml\\'" . web-mode)
                    ("\\.tpl\\.php\\'" . web-mode)
                    ("\\.jsp\\'" . web-mode)
                    ("\\.as[cp]x\\'" . web-mode)
                    ("\\.erb\\'" . web-mode)
                    ("\\.mustache\\'" . web-mode)
                    ("\\.djhtml\\'" . web-mode)
                    ("\\.html?\\'" . web-mode)
                    ("\\.coffee\\'" . coffee-mode)
                    ("\\.coffee.erb\\'" . coffee-mode)
                    ("\\.js.erb\\'" . js-mode)
                    ("\\.iced\\'" . coffee-mode)
                    ("\\.css\\'" . css-mode)
                    ("\\.wxss\\'" . css-mode)
                    ("Cakefile\\'" . coffee-mode)
                    ("\\.styl$" . sws-mode)
                    ("\\.jade" . jade-mode)
                    ("\\.go$" . go-mode)
                    ("\\.vala$" . vala-mode)
                    ("\\.vapi$" . vala-mode)
                    ("\\.rs$" . rust-mode)
                    ("\\.pro$" . qmake-mode)
                    ("\\.js$" . js-mode)
                    ("\\.wxs$" . js-mode)
                    ("\\.jsx$" . web-mode)
                    ("\\.lua$" . lua-mode)
                    ("\\.swift$" . swift-mode)
                    ("\\.l$" . flex-mode)
                    ("\\.y$" . bison-mode)
                    ("\\.pdf$" . pdf-view-mode)
                    ("\\.ts$" . typescript-mode)
                    ("\\.tsx$" . typescript-mode)
                    ("\\.cpp$" . c++-mode)
                    ("\\.h$" . c++-mode)
                    ("\\.ll$" . llvm-mode)
                    ("\\.bc$" . hexl-mode)
                    ("\\.nim$" . nim-mode)
                    ("\\.nims$" . nim-mode)
                    ("\\.nimble$" . nim-mode)
                    ("\\.nim.cfg$" . nim-mode)
                    ("\\.exs$" . elixir-mode)
                    ("\\.json$" . json-mode)
                    ("\\.clj$" . clojure-mode)
                    ("\\.dart$" . dart-mode)
                    ))
  (add-to-alist 'auto-mode-alist elt-cons))

(add-to-list 'interpreter-mode-alist '("coffee" . coffee-mode))


;;; Mode load.
(autoload 'cmake-mode "cmake-mode")
(autoload 'qml-mode "qml-mode")
(autoload 'markdown-mode "init-markdown-mode")
(autoload 'php-mode "php-mode")
(autoload 'web-mode "init-web-mode")
(autoload 'coffee-mode "coffee-mode")
(autoload 'sws-mode "sws-mode")
(autoload 'jade-mode "jade-mode")
(autoload 'css-mode "init-css-mode")
(autoload 'go-mode "init-golang")
(autoload 'vala-mode "vala-mode")
(autoload 'rust-mode "rust-mode")
(autoload 'qmake-mode "qmake-mode")
(autoload 'ruby-mode "init-ruby")
(autoload 'python-mode "init-python")
(autoload 'lua-mode "init-lua")
(autoload 'swift-mode "swift-mode")
(autoload 'haskell-mode "init-haskell")
(autoload 'js-mode "init-web-mode")
(autoload 'rjsx-mode "rjsx-mode")
(autoload 'flex-mode "flex")
(autoload 'bison-mode "bison")
(autoload 'llvm-mode "llvm-mode")
(autoload 'nim-mode "init-nim")
(autoload 'typescript-mode "typescript-mode")
(autoload 'elixir-mode "elixir-mode")
(autoload 'json-mode "json-mode")
(autoload 'clojure-mode "clojure-mode")
(autoload 'dart-mode "dart-mode")

;;; ### Auto-fill ###
;;; --- 自动换行
(setq default-fill-column 100)          ;默认显示 100列就换行
(dolist (hook (list
               'after-text-mode-hook
               'message-mode-hook
               ))
  (add-hook hook #'(lambda () (auto-fill-mode 1))))

(provide 'init-mode)