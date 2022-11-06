;; 窗口移动
(require 'winner)


(setq x-underline-at-descent-line t)
;; 窗口黄金比例

;; (require 'golden-ratio)
;; (golden-ratio-mode 1)

;; 绘制窗口线
(setq window-divider-default-places t
      window-divider-default-right-width 2
      window-divider-default-bottom-width 2)
(window-divider-mode t)

(provide 'init-window)
