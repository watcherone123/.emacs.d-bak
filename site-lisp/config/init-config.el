;; 加速配置。
(require 'init-accelerate)

;; 字体设置
(require 'init-font)


(with-temp-message ""              ;抹掉插件启动的输出
	;; (require 'init-fullscreen)

	(require 'init-generic)
	(require 'lazy-load)
	(require 'one-key)
	(require 'highlight-parentheses)

		;; 可以延后加载的
		(run-with-idle-timer
			1 nil
			#'(lambda ()
					(require 'init-awesome-tray)
				)))


(provide 'init-config)
