;; 加速配置。
(require 'init-accelerate)

;; 字体设置
(require 'init-font)


(with-temp-message ""              ;抹掉插件启动的输出
	;; (require 'init-fullscreen)

	(require 'init-generic)

	(require 'lazy-load)
	(require 'one-key)
	(require 'grammatical-edit)
	(require 'display-line-numbers)
	(require 'basic-toolkit)
	(require 'redo)

	(require 'highlight-parentheses)
	(require 'init-awesome-tray)
	(require 'init-line-number)
	(require 'init-lsp-bridge)
	(require 'init-auto-save)
	(require 'init-mode)
	(require 'init-grammatical-edit)
	(require 'init-indent)
	

	;; 可以延后加载的
	(run-with-idle-timer
		1 nil
		#'(lambda ()
				
			)))


(provide 'init-config)
