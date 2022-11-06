;;; Require
(require 'rime)

;;; Code:
;; (setq rime-user-data-dir "/home/sky/.config/fcitx/rime")

(setq rime-posframe-properties
      (list :background-color "#333333"
            :foreground-color "#dcdccc"
            :font "TsangerJinKai03-6763-13"
            :internal-border-width 10))

(setq default-input-method "rime"
      rime-show-candidate 'posframe)

(lazy-load-set-keys
 '(
   ("M-o" . rime--backspace)
   ("M-m" . rime--return)
   ("M-h" . rime--escape))
 rime-active-mode-map)

(provide 'init-rime)
