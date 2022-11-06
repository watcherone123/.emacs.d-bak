
;;; Code:

(dolist (hook (list
               'ielm-mode-hook
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'message-mode-hook
               'Info-mode-hook
               'erc-mode-hook
               'org-mode-hook
               ))
  (add-hook hook #'(lambda ()
                     (require 'eldoc)
                     (require 'eldoc-extension)
                     (setq eldoc-idle-delay 0) ;显示延迟
                     (setq eldoc-argument-case 'eldoc-argument-list) ;高亮函数参数
                     (turn-on-eldoc-mode)
                     )))

(provide 'init-eldoc)