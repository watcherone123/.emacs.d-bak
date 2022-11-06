(require 'vundo)
(add-hook 'vundo-mode-hook
	  (lambda ()
	    (+sky/meow-add-motion-mode-alist 'vundo-mode)
	    (meow-define-keys
		'motion
	      '("h" . vundo-backward)
	      '("l" . vundo-forward)
	      '("j" . vundo-next)
	      '("k" . vundo-previous))
	    ))
(provide 'init-vundo)
