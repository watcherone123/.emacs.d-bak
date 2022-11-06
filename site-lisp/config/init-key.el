
;;; ### Unset key ###
;;; --- 卸载按键
(lazy-load-unset-keys                   ;全局按键的卸载
 '("C-x C-f" "C-z" "C-q" "s-W" "s-z" "M-h" "C-x C-c" "C-\\" "s-c" "s-x" "s-v" "C-6"))
;;; ### Popweb ###
;;; --- Web翻译
(lazy-load-global-keys
 '((";" . popweb-dict-bing-input)
   ("y" . popweb-dict-bing-pointer))
 "init-popweb"
 "C-z")
;;; ### Insert translated name ###
(lazy-load-global-keys
 '(
   ("," . insert-translated-name-insert-with-underline)
   ("." . insert-translated-name-insert-with-camel)
   )
 "insert-translated-name"
 "C-z"
 )
(lazy-load-global-keys
 '(
   ("s-i" . insert-translated-name-insert)
   )
 "init-insert-translated-name")
;; Dash.
(lazy-load-global-keys
 '(("y" . dash-at-point)
   )
 "dash-at-point"
 "C-x"
 )
;;; ### Toolkit ###
;;; --- 工具函数
(lazy-load-set-keys
 '(
   ("s-c o" . one-key-menu-directory)   ;目录打开菜单
   ("s-," . bury-buffer)                ;隐藏当前buffer
   ("s-." . unbury-buffer)              ;反隐藏当前buffer
   ("s-[" . eval-expression)            ;执行表达式
   ("C-s-q" . quoted-insert)            ;读取系一个输入字符并插入
   ("M-h" . set-mark-command) ;Instead C-Space for Chinese input method
   ("M-H" . set-mark-command) ;Instead C-Space for Chinese input method
   ("M-;" . comment-dwim)
   ))
(lazy-load-global-keys
 '(
   ("s-R" . re-builder)                 ;可视化构建正则表达式
   )
 "init-rebuilder")
;;; ### Color-Rg ###
;;; --- 搜索重构
(lazy-load-global-keys
 '(
   ("s-x g" . color-rg-search-symbol)
   ("s-x h" . color-rg-search-input)
   ("s-x j" . color-rg-search-symbol-in-project)
   ("s-x k" . color-rg-search-input-in-project)
   ("s-x ," . color-rg-search-symbol-in-current-file)
   ("s-x ." . color-rg-search-input-in-current-file)
   )
 "color-rg")
(lazy-load-global-keys
 '(
   ("C-z l" . display-line-numbers-mode) ;行号模式切换
   ("M-s-n" . comment-part-move-down)    ;向下移动注释
   ("M-s-p" . comment-part-move-up)      ;向上移动注释
   ("C-s-n" . comment-dwim-next-line)    ;移动到上一行并注释
   ("C-s-p" . comment-dwim-prev-line)    ;移动到下一行并注释
   ("M-2" . indent-buffer)               ;自动格式化当前Buffer
   ("M-z" . upcase-char)      ;Upcase char handly with capitalize-word
   ("C-x u" . mark-line)      ;选中整行
   ("s-k" . kill-and-join-forward)      ;在缩进的行之间删除
   ("M-G" . goto-column)                ;到指定列
   ("C->" . remember-init)              ;记忆初始函数
   ("C-<" . remember-jump)              ;记忆跳转函数
   ("M-s-," . point-stack-pop)          ;buffer索引跳转
   ("M-s-." . point-stack-push)         ;buffer索引标记
   ("s-g" . goto-percent)    ;跳转到当前Buffer的文本百分比, 单位为字符
   ("M-I" . backward-indent) ;向后移动4个字符
   ("s-J" . scroll-up-one-line)         ;向上滚动一行
   ("s-K" . scroll-down-one-line)       ;向下滚动一行
   ("<f2>" . refresh-file)              ;自动刷新文件
   ("s-f" . find-file-root)             ;用root打开文件
   ("s-r" . find-file-smb)              ;访问sambao
   )
 "basic-toolkit")
(lazy-load-global-keys
 '(
   ("M-g" . goto-line-preview))
 "goto-line-preview")
;;; ### Delete block ###
;;; --- 快速删除光标左右的内容
(lazy-load-global-keys
 '(
   ("M-N" . delete-block-backward)
   ("M-M" . delete-block-forward))
 "delete-block")
;;; ### Watch other window ###
;;; --- 滚动其他窗口
(lazy-load-global-keys
 '(
   ("M-J" . watch-other-window-up)        ;向下滚动其他窗口
   ("M-K" . watch-other-window-down)      ;向上滚动其他窗口
   ("M-<" . watch-other-window-up-line)   ;向下滚动其他窗口一行
   ("M->" . watch-other-window-down-line) ;向上滚动其他窗口一行
   )
 "watch-other-window")
;;; ### Buffer Move ###
;;; --- 缓存移动
(lazy-load-set-keys
 '(
   ("C-z k" . beginning-of-buffer)      ;缓存开始
   ("C-z j" . end-of-buffer)            ;缓存结尾
   ("C-M-f" . forward-paragraph)        ;下一个段落
   ("C-M-b" . backward-paragraph)       ;上一个段落
   ("C-M-y" . backward-up-list)         ;向左跳出 LIST
   ("C-M-o" . up-list)                  ;向右跳出 LIST
   ("C-M-u" . backward-down-list)       ;向左跳进 LIST
   ("C-M-i" . down-list)                ;向右跳进 LIST
   ("C-M-a" . beginning-of-defun)       ;函数开头
   ("C-M-e" . end-of-defun)             ;函数末尾
   ))
(lazy-load-global-keys
 '(
   ("M-s" . symbol-overlay-put)         ;懒惰搜索
   )
 "init-symbol-overlay")
(lazy-load-global-keys
 '(
   ("s-N" . move-text-down)      ;把光标所在的整行文字(或标记)下移一行
   ("s-P" . move-text-up)        ;把光标所在的整行文字(或标记)上移一行
   )
 "move-text")
(lazy-load-global-keys
 '(
   ("C-S-o" . duplicate-line-or-region-above) ;向上复制当前行或区域
   ("C-S-l" . duplicate-line-or-region-below) ;向下复制当前行或区域
   ("C-S-s-o" . duplicate-line-above-comment) ;复制当前行到上一行, 并注释当前行
   ("C-S-s-l" . duplicate-line-below-comment) ;复制当前行到下一行, 并注释当前行
   ("C-:" . comment-or-uncomment-region+)     ;注释当前行
   )
 "duplicate-line")
(lazy-load-global-keys
 '(
   ("C-o" . open-newline-above)         ;在上面一行新建一行
   ("C-l" . open-newline-below)         ;在下面一行新建一行
   )
 "open-newline")
;;; ### Buffer Name ###
;;; --- 缓存名字
(lazy-load-global-keys
 '(
   ("C-M-;" . kill-other-window-buffer) ;关闭其他窗口的buffer
   )
 "buffer-extension")
;;; ### Buffer Edit ###
;;; --- 缓存编辑
(lazy-load-set-keys
 '(
   ("C-x C-x" . exchange-point-and-mark)   ;交换当前点和标记点
   ("M-o" . backward-delete-char-untabify) ;向前删除字符
   ("C-M-S-h" . mark-paragraph)            ;选中段落
   ("M-SPC" . just-one-space)              ;只有一个空格在光标处
   ))
(lazy-load-global-keys
 '(
   ("C-/" . undo-tree-undo)             ;撤销
   ("C-?" . undo-tree-redo)             ;重做
   )
 "undo-tree")
(lazy-load-global-keys
 '(
   ("C-," . goto-last-change)           ;跳到最后编辑的地方
   )
 "goto-last-change")
;;; ### Rect ###
;;; --- 矩形操作
(lazy-load-global-keys
 '(
   ("s-M" . rm-set-mark)                ;矩形标记
   ("s-X" . rm-exchange-point-and-mark) ;矩形对角交换
   ("s-D" . rm-kill-region)             ;矩形删除
   ("s-S" . rm-kill-ring-save)          ;矩形保存
   ("s-Y" . yank-rectangle)             ;粘帖矩形
   ("s-O" . open-rectangle)            ;用空白填充矩形, 并向右移动文本
   ("s-C" . clear-rectangle)           ;清空矩形
   ("s-T" . string-rectangle)          ;用字符串替代矩形的每一行
   ("s-I" . string-insert-rectangle)   ;插入字符串在矩形的每一行
   ("s-F" . delete-whitespace-rectangle) ;删除矩形中空格
   ("s-\"" . copy-rectangle-to-register) ;拷贝矩形到寄存器
   ("s-:" . mark-rectangle-to-end)       ;标记矩形到行末
   )
 "rect-extension")
;;; ### Font ###
;;; --- 字体命令
(lazy-load-set-keys
 '(
   ("s--" . text-scale-decrease)        ;减小字体大小
   ("s-=" . text-scale-increase)        ;增加字体大小
   ))
;;; ### 调整数字 ###
;;; --- 调整光标处数字
(lazy-load-global-keys
 '(
   ("M--" . shift-number-down)
   ("M-=" . shift-number-up))
 "shift-number")
;;; ### Window Operation ###
;;; --- 窗口操作
(lazy-load-set-keys
 '(
   ("C-c v" . split-window-vertically)   ;纵向分割窗口
   ("C-c h" . split-window-horizontally) ;横向分割窗口
   ("C-x ;" . delete-other-windows)      ;关闭其它窗口
   ))
(lazy-load-global-keys
 '(
   ("C-'" . delete-current-buffer-and-window) ;关闭当前buffer, 并关闭窗口
   ("C-\"" . delete-current-buffer-window)    ;删除当前buffer的窗口
   ("C-x O" . toggle-window-split)
   )
 "window-extension")
;;; ### Toggle-One-Window ###
;;; --- 临时最大化当前窗口
(lazy-load-global-keys
 '(
   ("M-s-o" . toggle-one-window)        ;切换一个窗口
   )
 "toggle-one-window")
;;; ### Sort-Tab ###
;;; --- 多标签浏览
(lazy-load-global-keys
 '(
   ("M-7" . sort-tab-select-prev-tab)    ;选择前一个标签
   ("M-8" . sort-tab-select-next-tab)    ;选择后一个标签
   ("M-s-7" . sort-tab-select-first-tab) ;选择第一个标签
   ("M-s-8" . sort-tab-select-last-tab)  ;选择最后一个标签
   ("C-;" . sort-tab-close-current-tab)  ;关闭当前标签
   ("s-q" . sort-tab-close-mode-tabs)    ;关闭特定模式的标签
   ("s-Q" . sort-tab-close-all-tabs)     ;关闭所有标签
   )
 "sort-tab")
;;; ### Functin key ###
;;; --- 功能函数
(lazy-load-set-keys
 '(
   ("<f5>" . emacs-session-save)        ;退出emacs
   ("C-4" . insert-changelog-date)      ;插入日志时间 (%Y/%m/%d)
   ("C-&" . switch-to-messages)         ;跳转到 *Messages* buffer
   ))
;;; ### Grammatical-Edit ###
;;; --- 结构化编程
(lazy-load-unset-keys
 '("M-J" "M-r" "M-s" "M-;" "C-M-f" "C-M-b" "M-)")
 grammatical-edit-mode-map)             ;卸载按键
(defvar grammatical-edit-key-alist nil)
(setq grammatical-edit-key-alist
      '(
        ;; 移动
        ("M-n" . grammatical-edit-jump-left)
        ("M-p" . grammatical-edit-jump-right)
        ;; 符号插入
        ("%" . grammatical-edit-match-paren)       ;括号跳转
        ("(" . grammatical-edit-open-round)        ;智能 (
        ("[" . grammatical-edit-open-bracket)      ;智能 [
        ("{" . grammatical-edit-open-curly)        ;智能 {
        (")" . grammatical-edit-close-round)       ;智能 )
        ("]" . grammatical-edit-close-bracket)     ;智能 ]
        ("}" . grammatical-edit-close-curly)       ;智能 }
        ("\"" . grammatical-edit-double-quote)     ;智能 "
        ("'" . grammatical-edit-single-quote)      ;智能 '
        ("=" . grammatical-edit-equal)             ;智能 =
        ("SPC" . grammatical-edit-space)           ;智能 space
        ("RET" . grammatical-edit-newline)         ;智能 newline
        ;; 删除
        ("M-o" . grammatical-edit-backward-delete) ;向后删除
        ("C-d" . grammatical-edit-forward-delete)  ;向前删除
        ("C-k" . grammatical-edit-kill)            ;向前kill
        ;; 包围
        ("M-\"" . grammatical-edit-wrap-double-quote) ;用 " " 包围对象, 或跳出字符串
        ("M-'" . grammatical-edit-wrap-single-quote) ;用 ' ' 包围对象, 或跳出字符串
        ("M-[" . grammatical-edit-wrap-bracket)      ;用 [ ] 包围对象
        ("M-{" . grammatical-edit-wrap-curly)        ;用 { } 包围对象
        ("M-(" . grammatical-edit-wrap-round)        ;用 ( ) 包围对象
        ("M-)" . grammatical-edit-unwrap)            ;去掉包围对象
        ;; 跳出并换行缩进
        ("M-:" . grammatical-edit-jump-out-pair-and-newline) ;跳出括号并换行
        ;; 向父节点跳动
        ("C-j" . grammatical-edit-jump-up)
        ))
(lazy-load-set-keys grammatical-edit-key-alist grammatical-edit-mode-map)
;;; ### Thingh-edit ###
;;; --- 增强式编辑当前光标的对象
(lazy-load-global-keys
 '(
   ("M-s-h" . one-key-menu-thing-edit)  ;thing-edit 菜单
   )
 "init-thing-edit"
 )
;;; ### Aweshell ###
;;; --- 多标签式的shell
(lazy-load-global-keys
 '(
   ("s-n" . aweshell-new)
   ("s-h" . aweshell-toggle)
   ("s-x s-x" . aweshell-dedicated-toggle)
   )
 "aweshell")
;;; ### EAF ###
;;; EAF
(unless (featurep 'cocoa)
  (lazy-load-global-keys
   '(
     ("M-j" . eaf-open-in-file-manager)
     ("s-'" . eaf-open)
     ("s-\"" . eaf-open-browser)
     ("s-/" . eaf-open-terminal)
     ("s-b" . eaf-open-rss-reader)
     )
   "init-eaf"))
;; Cycle buffer
(lazy-load-global-keys
 '(
   ("M-C" . one-key-menu-cycle-buffer)  ;特定模式切换
   )
 "init-cycle-buffer")
;;; ### Isearch ###
;;; --- 交互式搜索
(lazy-load-set-keys
 '(
   ("TAB" . isearch-complete)           ;isearch补全
   ("C-s" . isearch-repeat-forward) ;重复向前搜索, 第一次可以用来搜索上一次的历史哟
   ("C-r" . isearch-repeat-backward)   ;重复向后搜索
   ("C-g" . isearch-abort)             ;中止搜索
   ("C-w" . isearch-yank-word-or-char) ;粘帖光标后的词或字符作为搜索对象
   ("C-y" . isearch-yank-line)         ;粘帖光标后的行作为搜索对象
   ("M-o" . isearch-delete-char)       ;删除
   ("M-p" . isearch-ring-retreat)      ;搜索历史向后
   ("M-n" . isearch-ring-adjust)       ;搜索历史向前
   ("M-y" . isearch-yank-kill) ;从 kill ring 中粘帖最后一项到搜索对象后
   ("M-h" . isearch-yank-char) ;粘帖光标后的字符到搜索对象
   ("M-e" . isearch-edit-string)        ;编辑搜索对象
   ("M-c" . isearch-toggle-case-fold)   ;切换大小写
   ("M-r" . isearch-toggle-regexp)      ;切换正则表达式
   ("M-w" . isearch-toggle-word)        ;切换词
   ("M->" . isearch-beginning-of-buffer) ;跳转到buffer开头并重新搜索, 搜索最前面一个
   ("M-<" . isearch-end-of-buffer) ;跳转到buffer末尾并重新搜索, 搜索最后面一个
   ("M-%" . isearch-query-replace) ;替换
   ("M-d" . isearch-find-duplicate-word)    ;查找重复的单词
   ("M-z" . isearch-find-duplicate-line)    ;查找重复的行
   ("C-M-%" . isearch-query-replace-regexp) ;正则表达式替换
   )
 isearch-mode-map
 )
;;; ### kill-ring-search ###
;;; --- 删除环的递增式搜索
(lazy-load-global-keys
 '(
   ("M-s-y" . kill-ring-search)         ;kill ring 搜索
   )
 "init-kill-ring-search")
;;; ### Help ###
;;; --- 帮助模式
(lazy-load-global-keys
 '(
   ("C-h". one-key-menu-help)           ;帮助菜单
   )
 "init-help-mode")
(lazy-load-global-keys
 '(
   ("M-U" . smart-align)
   )
 "smart-align")
;;; ### Yoaddmuse ###
;;; --- Yet another oddmuse mode
(lazy-load-global-keys
 '(
   ("M-s-;" . one-key-menu-yaoddmuse)   ;yaoddmuse 菜单
   )
 "init-yaoddmuse")
;;; ### Festival ###
;;; --- 语音阅读
(lazy-load-global-keys
 '(
   ("s-x r" . one-key-menu-festival)    ;语音阅读菜单
   )
 "init-festival")
;;; ### Less ###
;;; --- 快速浏览模式
(lazy-load-global-keys
 '(
   ("M-s-l" . less-minor-mode)          ;打开less模式
   )
 "init-less")
;;; ### iedit ###
;;; --- iedit
(lazy-load-global-keys
 '(
   ("s-o" . iedit-mode)
   )
 "init-iedit")
;;; ### Ace jump ###
(lazy-load-global-keys
 '(
   ("s-<" . ace-jump-word-mode)
   ("s->" . ace-jump-char-mode)
   ("s-?" . ace-jump-line-mode)
   )
 "ace-jump-mode")
;;; ### Python ###
;;; --- Python mode
(eval-after-load 'python-mode
  '(lambda ()
     (lazy-load-local-keys
      '(
        ("C-S-j" . jump-to-import)
        )
      python-mode-map
      "python-mode-utils")
     ))
;;; ### Ielm ###
;;; --- Emacs Lisp 解释模式
(autoload 'ielm-map "ielm")
(lazy-load-global-keys
 '(
   ("M-s-i" . ielm-toggle)              ;切换ielm
   ("s-p" . insert-standard-date)
   )
 "lazycat-toolkit")
(eval-after-load 'ielm-mode
  '(lambda ()
     (progn
       (lazy-load-unset-keys
        '("M-p" "M-n")
        ielm-map)                       ;卸载按键
       (lazy-load-set-keys
        '(
          ("C-s-p" . comint-previous-input) ;上一个输入
          ("C-s-n" . comint-next-input)     ;下一个输入
          )
        ielm-map
        )
       )))
;;; ### Man ###
;;; --- Man
(lazy-load-global-keys
 '(
   ("<f1>" . woman))
 "init-woman")
;;; ### Company en words ###
;;; --- 英文助手
(lazy-load-global-keys
 '(
   ("M-r" . lsp-bridge-toggle-sdcv-helper) ;英文助手
   )
 "init-lsp-bridge")
;;; ### Ido ###
;;; --- 交互式管理文件和缓存
(lazy-load-set-keys
 '(
   ("C-x C-f" . ido-find-file)          ;交互式查找文件
   ("C-x b" . ido-switch-buffer)        ;交互式切换buffer
   ("C-x i" . ido-insert-buffer)        ;插入缓存
   ("C-x I" . ido-insert-file)          ;插入文件
   ))
(add-hook 'ido-setup-hook
          #'(lambda ()
              (interactive)
              (ido-my-keys ido-completion-map)))
(defun ido-my-keys (keymap)
  "Add my keybindings for ido."
  (lazy-load-set-keys
   '(
     ("M-s-p" . ido-prev-match)              ;上一个匹配
     ("M-s-n" . ido-next-match)              ;下一个匹配
     ("M-s-h" . ido-next-work-directory)     ;下一个工作目录
     ("M-s-l" . ido-prev-work-directory)     ;上一个工作目录
     ("M-o" . backward-delete-char-untabify) ;向前删除字符
     ("M-O" . ido-delete-backward-updir)     ;删除字符或进入上一级目录
     )
   keymap
   ))
;;; ### IRC ###
;;; --- 聊天
(lazy-load-global-keys
 '(
   ("C-c i" . switch-to-erc)            ;切换到IRC或自动登录IRC
   ("C-c I" . erc-nick-notify-jump-last-channel) ;自动跳转到最后收到消息的频道
   )
 "init-erc")
;;; Elisp
(lazy-load-set-keys
 '(
   ("RET" . comment-indent-new-line)    ;自动换行并注释
   )
 emacs-lisp-mode-map
 )
;;; ### Org ###
;;; --- 笔记管理和组织
(lazy-load-global-keys
 '(
   ("s-s" . one-key-menu-org-file)      ;Org 文件
   ("C-c r" . org-remember)             ;Org-remeber
   )
 "init-org-mode")
;;; ### String Inflection ###
;; --- 单词语法风格快速转换
(lazy-load-global-keys
 '(
   ("C-c C-u" . one-key-string-inflection)
   )
 "init-string-inflection")
;;; ### Projectile Rails ###
;; Rails 文件快速导航
(lazy-load-global-keys
 '(
   ("s-c p" . one-key-projectile-rails) ;projectile rails
   )
 "init-projectile-rails")
;;; ### Keyboard Macro ###
;;; --- 键盘宏
(lazy-load-global-keys
 '(
   ("M-s-s" . kmacro-start-macro-or-insert-counter) ;开始键盘宏或插入
   ("M-s-d" . kmacro-end-or-call-macro)             ;结束键盘宏或调用
   ("M-s-c" . kmacro-delete-ring-head)              ;删除当前的键盘宏
   ("M-s-w" . kmacro-cycle-ring-next)               ;下一个键盘宏
   ("M-s-e" . kmacro-cycle-ring-previous)           ;上一个键盘宏
   ("M-s-a" . kmacro-edit-macro)                    ;编辑键盘宏
   ("M-s-v" . name-last-kbd-macro)                  ;命令当前键盘宏
   ("M-s-f" . insert-kbd-macro)                     ;插入键盘宏
   ("M-s-q" . apply-macro-to-region-lines) ;应用键盘宏到选择的区域
   )
 "macros+")
;;; ### auto-install ###
(lazy-load-global-keys
 '(
   ("C-s-x" . auto-install-from-emacswiki))
 "init-auto-install")
;;; ### Git ###
;;
(lazy-load-global-keys
 '(
   ("s-x f" . one-key-menu-git))
 "init-eaf")

;;; ### Input Method ###
(lazy-load-global-keys
 '(
   ("s-m" . toggle-input-method)
   )
 "init-rime")

(lazy-load-global-keys
 '(
   ("M-x" . smex+)
   ("C-c C-c M-x" . execute-extended-command)
   )
 "init-smex")

(lazy-load-global-keys
 '(
   ("C-M-%" . vr/query-replace))
 "init-visual-regexp")

(lazy-load-global-keys
 '(
   ("s-y" . blink-search)
   )
 "init-blink-search")

(lazy-load-unset-keys                   ;全局按键的卸载
 '("M-." "M-,"))
(lazy-load-global-keys
 '(
   ("C-7" . lsp-bridge-jump-back)
   ("C-8" . lsp-bridge-jump)
   ("M-," . lsp-bridge-code-action)
   ("M-." . lsp-bridge-find-references)
   ("C-9" . lsp-bridge-popup-documentation)
   ("C-0" . lsp-bridge-rename)
   ("M-s-j" . lsp-bridge-diagnostic-jump-next) ;显示下一个错误
   ("M-s-k" . lsp-bridge-diagnostic-jump-prev) ;显示上一个错误
   ("M-s-l" . lsp-bridge-diagnostic-ignore)    ;忽略当前的错误
   ("M-s-u" . lsp-bridge-diagnostic-copy)      ;拷贝诊断信息
   ("M-s-n" . lsp-bridge-popup-documentation-scroll-up) ;向下滚动文档
   ("M-s-p" . lsp-bridge-popup-documentation-scroll-down) ;向上滚动文档
   )
 "init-lsp-bridge")

(lazy-load-global-keys
 '(
   ("C-6" . telega)
   )
 "init-telega")

(lazy-load-global-keys
 '(
   ("C--" . recursive-search-references)
   )
 "recursive-search-references")

(provide 'init-key)