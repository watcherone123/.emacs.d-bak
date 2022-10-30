;; Restore emacs session.
(setq initial-buffer-choice t)
;; (run-with-timer 1 nil #'(lambda () (bury-buffer)))

;; 增加长行处理性能
(setq bidi-inhibit-bpa t)
(setq-default bidi-paragraph-direction 'left-to-right)

(provide 'init-generic)
