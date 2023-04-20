;;; init-which-key.el --- init-editor config -*- lexical-binding: t -*-



(add-hook 'after-init-hook (lambda ()
                             (require 'which-key)
                             (which-key-mode 1)
                             (with-eval-after-load 'which-key
                               (setq which-key-idle-delay 0.3
                                     which-key-compute-remaps t
                                     which-key-min-display-lines 1
                                     which-key-add-column-padding 1
                                     which-key-max-display-columns nil
                                     which-key-sort-uppercase-first nil
                                     which-key-side-window-max-width 0.33
                                     which-key-side-window-max-height 0.20
                                     which-key-sort-order #'which-key-prefix-then-key-order)
                               (which-key-setup-side-window-bottom)
                               (dolist (item '((("SPC" . nil) . ("␣" . nil))
                                               (("TAB" . nil) . ("↹" . nil))
                                               (("RET" . nil) . ("⏎" . nil))
                                               (("DEL" . nil) . ("⌫" . nil))
                                               (("<up>" . nil) . ("↑" . nil))
                                               (("<down>" . nil) . ("↓" . nil))
                                               (("<left>" . nil) . ("←" . nil))
                                               (("<right>" . nil) . ("→" . nil))
                                               (("deletechar" . nil) . ("⌦" . nil))
                                               ;; rename winum-select-window-1 entry to 1..9
                                               (("\\(.*\\)1" . "winum-select-window-1") . ("\\11..9" . "window 1..9"))
                                               ;; hide winum-select-window-[2-9] entries
                                               ((nil . "winum-select-window-[2-9]") . t)))
                                 (cl-pushnew item which-key-replacement-alist :test #'equal))
                               (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
                               )
                             ))





(provide 'init-which-key)
