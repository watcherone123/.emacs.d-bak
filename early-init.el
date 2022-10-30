;; 增加IO性能
(setq read-process-output-max (* 10240 1024))
(setq process-adaptive-read-buffering nil)
(setq gc-cons-percentage 1.0)
(setq gc-cons-threshold most-positive-fixnum)
(require 'cl-lib)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(scroll-bar-mode -1)


(add-to-list 'load-path "~/.emacs.d/site-lisp/pkg/lazycat-theme")
(add-to-list 'load-path "~/.emacs.d/site-lisp/pkg/themes")
(require 'lazycat-theme)
(require 'doom-themes)

(load-theme 'doom-tokyo-night t t)
