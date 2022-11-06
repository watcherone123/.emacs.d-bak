;; 增加IO性能
(setq read-process-output-max (* 10240 1024))
(setq process-adaptive-read-buffering nil)
(setq gc-cons-percentage 1.0)
(setq gc-cons-threshold most-positive-fixnum)
(require 'cl-lib)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(scroll-bar-mode -1)
(setq byte-compile-warnings '(cl-functions)) ;;去除cl警告

(add-to-list 'load-path "~/.emacs.d/site-lisp/pkg/lazycat-theme")

(require 'lazycat-theme)
(lazycat-theme-load-dark)

