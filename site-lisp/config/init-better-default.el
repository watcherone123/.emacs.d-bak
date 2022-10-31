(setq user-full-name "watcherone123")       ;设置用户名
(setq user-mail-address
      "watcherone123@gmail.com") 	;设置邮箱
(setq use-dialog-box nil)               ;never pop dialog
(setq inhibit-startup-screen nil)         ;inhibit start screen
(setq initial-scratch-mbessage "")      ;关闭启动空白buffer, 这个buffer会干扰session恢复
;; (setq-default comment-style 'indent)    ;设定自动缩进的注释风格
(setq ring-bell-function 'ignore)       ;关闭烦人的出错时的提示声
;; (setq default-major-mode 'text-mode)    ;设置默认地主模式为TEXT模式
(setq mouse-yank-at-point t)            ;粘贴于光标处,而不是鼠标指针处
(setq x-select-enable-clipboard t)      ;支持emacs和外部程序的粘贴 x11
(setq select-enable-clipboard t)        ; wayland 复制粘贴共享
(setq split-width-threshold nil)        ;分屏的时候使用上下分屏
(setq inhibit-compacting-font-caches t) ;使用字体缓存，避免卡顿
(setq confirm-kill-processes t)       ;退出自动杀掉进程
(setq async-bytecomp-allowed-packages nil) ;避免magit报错
(setq word-wrap-by-category t)             ;按照中文折行
(setq garbage-collection-messages t)	;gc时显示消息
(setq byte-compile-warnings nil)	;关闭字节编译警告
(setq warning-minimum-log-level :debug)
(setq custom-file "~/.emacs.d/site-lisp/custom.el")
(setq ad-redefinition-action 'accept)   ;不要烦人的 redefine warning
(setq frame-resize-pixelwise t) ;设置缩放的模式,避免Mac平台最大化窗口以后右边和下边有空隙

(add-to-list 'after-make-frame-functions
	     (lambda (new-frame)
	       (select-frame new-frame)
		;; English Font
		(set-face-attribute 'default nil :font "Fira Mono 12")
		;; Chinese Font
		(dolist (charset '(kana han symbol cjk-misc bopomofo))
		  (set-fontset-font (frame-parameter nil 'font)
		         charset (font-spec :family "Noto Sans CJK SC" :size 16)))))


;; 平滑地进行半屏滚动，避免滚动后recenter操作
(setq scroll-step 1
      scroll-conservatively 10000)
;; 不需要使用默认的自动保存
(setq auto-save-default nil)
;; 关闭自动备份文件
(setq make-backup-files nil)
;; 关闭启动页
(setq inhibit-startup-screen t)
(setq scroll-margin 5); 设定滚动边距





(add-hook 'prog-mode-hook (lambda ()
			    ;; (display-line-numbers-mode t)
			    ;; 不要自动换行
			    (setq truncate-lines t)))

;; (global-visual-line-mode t)
(global-auto-revert-mode t); 开启自动重载buffer
(setq-default recentf-max-saved-items 1000)
(recentf-mode t); 保存最近打开的文件
(winner-mode t); 操作窗口模式

(fset 'yes-or-no-p 'y-or-n-p)           ;以 y/n代表 yes/no
(blink-cursor-mode -1)                  ;指针不闪动
(transient-mark-mode 1)                 ;标记高亮
(global-subword-mode 1)                 ;Word移动支持 FooBar 的格式
(save-place-mode 1)			;记住光标的位置





;; 自定义 *scratch* 内容
(defun +sky/scratch-setup()
  (interactive)
  (save-excursion
    (with-current-buffer (get-buffer "*scratch*")
      (erase-buffer)
      (insert (format "启动时长: %s" (emacs-init-time)))
      (insert "\n")
      (insert-button "Quit Emacs"
		     'action (lambda (_button)
			       (save-buffers-kill-emacs)))
      (insert "\n")
      (insert "Recent Files\n")
      (dolist (f recentf-list)
	(insert-button f
		       'action (lambda (region)
				 (require 'f)
				 (let* ((f (buffer-substring-no-properties (overlay-start region) (overlay-end region)))
					(fname (f-filename f)))
				   (find-file-noselect f)
				   (switch-to-buffer fname))))
	(insert "\n"))
      )))


(provide 'init-better-default)
