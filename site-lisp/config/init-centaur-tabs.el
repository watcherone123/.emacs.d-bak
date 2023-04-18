;;; init-tabs.el --- init-editor config -*- lexical-binding: t -*-

(require 'centaur-tabs)

(add-hook 'after-init-hook #'centaur-tabs-mode)
(add-hook 'dired-mode-hook #'centaur-tabs-local-mode)

(with-eval-after-load 'centaur-tabs
    (setq centaur-tabs-cycle-scope 'tabs) ; Navigate through visible tabs only.
  (setq centaur-tabs-style "chamfer")
  (setq centaur-tabs-height 20)
  (setq centaur-tabs-set-bar 'under)
  ;; (setq centaur-tabs-set-icons t)
  ;; Note: If you're not using Spacmeacs, in order for the underline to display
  ;; correctly you must add the following line:
  (setq x-underline-at-descent-line t)
  )

(provide 'init-centaur-tabs)
