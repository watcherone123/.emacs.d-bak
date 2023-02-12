
;;; Require
(require 'awesome-tray)

;;; Code:
(defun meow-module-info()
  (meow-indicator))
(defface meow-module-face ()
  ""
  :group 'awesome-tray)

(add-to-list 'awesome-tray-module-alist
	     '("meow" . (meow-module-info meow-module-face)))

(setq awesome-tray-active-modules '("meow" "location" "file-path" "buffer-name" "mode-name" "date"))


;; 为了让awesome-tray内容不要换行
(setq awesome-tray-info-padding-right 5)

(awesome-tray-mode 1)

(provide 'init-awesome-tray)
