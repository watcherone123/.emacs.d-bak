;; 打开emacs后必须立刻使用的

(lazycat-theme-load-dark)
(require 'init-generic)


(require 'lazy-load)
(require 'one-key)
;; 打开窗口后再使用
(add-hook 'window-setup-hook (lambda ()

			       (require 'init-better-default)
			       (require 'init-awesome-tray)
			      ;; (+sky/set-fonts)
			       (+sky/scratch-setup)))

(provide 'init-config)
