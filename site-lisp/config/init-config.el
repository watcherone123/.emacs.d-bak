;; 加速配置。
(require 'init-accelerate)

;; 字体设置
(require 'init-font)

;; 定义一些启动目录，方便下次迁移修改
(defvar sky-emacs-root-dir (file-truename "~/.emacs.d/site-lisp"))
(defvar sky-emacs-config-dir (concat sky-emacs-root-dir "/config"))
(defvar sky-emacs-extension-dir (concat sky-emacs-root-dir "/pkg"))

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
  (require 'init-one-key)
  (require 'init-key)
  (require 'init-vi-navigate)
  (require 'init-isearch-mb)
  (require 'init-performance)
  ;; (require 'init-rime) TODO:带解决问题


  ;; 可以延后加载的
  (run-with-idle-timer
   1 nil
   #'(lambda ()
       (require 'pretty-lambdada)
       (require 'browse-kill-ring)
       (require 'elf-mode)

       (require 'init-tree-sitter)
       (require 'init-eldoc)
       (require 'init-yasnippet)
       (require 'init-cursor-chg)
       (require 'init-winpoint)
       (require 'init-info)
       (require 'init-c)
       (require 'init-org)
       (require 'init-idle)
       (require 'init-markdown-mode)

       (require 'init-eaf)

       (require 'init-session)
       (emacs-session-restore)

       (require 'init-sort-tab)

       )))


(provide 'init-config)
