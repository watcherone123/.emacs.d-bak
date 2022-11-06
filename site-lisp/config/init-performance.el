;; Disable garbage collection when entering commands.
(defun max-gc-limit ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun reset-gc-limit ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'max-gc-limit)
(add-hook 'minibuffer-exit-hook #'reset-gc-limit)

;; Improve the performance of rendering long lines.
(setq-default bidi-display-reordering nil)

(provide 'init-performance)