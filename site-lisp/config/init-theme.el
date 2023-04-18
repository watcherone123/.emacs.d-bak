;;; init-theme.el - -*- lexical-binding: t -*-

;;; Code:
(require 'ef-themes)

 ;; set two specific themes and switch between them
  (setq ef-themes-to-toggle '(ef-spring ef-bio))
  ;; set org headings and function syntax
  (setq ef-themes-headings
        '((0 . (bold 1))
          (1 . (bold 1))
          (2 . (rainbow bold 1))
          (3 . (rainbow bold 1))
          (4 . (rainbow bold 1))
          (t . (rainbow bold 1))))
  (setq ef-themes-region '(intense no-extend neutral))
   ;; Disable all other themes to avoid awkward blending:
  (mapc #'disable-theme custom-enabled-themes)
(add-hook 'after-init-hook #'(lambda()(ef-themes-select 'ef-bio)))
;; (ef-themes-select 'ef-bio)
(provide 'init-theme)
