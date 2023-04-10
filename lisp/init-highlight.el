;; init-highlight.el --- Initialize highlighting configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'init-funcs)

;; Highlight the current line
(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))

(use-package symbol-overlay
  :diminish
  :ensure t
  :hook (prog-mode . symbol-overlay-mode)
  :init (setq symbol-overlay-idle-time 0.1))

(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :init (setq show-paren-when-point-inside-paren nil
              show-paren-when-point-in-periphery t))

(use-package highlight-parentheses
  :ensure t
  :hook (prog-mode . highlight-parentheses-mode)
  :config
  (setq highlight-parentheses-delay 0.2)
  (setq highlight-parentheses-colors '("Springgreen3"
                                       "IndianRed3"
                                       "IndianRed4"
                                       "yellow"
                                       )))


;; Highlight uncommitted changes using VC
(use-package diff-hl
  :bind (:map diff-hl-command-map
         ("SPC" . diff-hl-mark-hunk))
  :hook (after-init . global-diff-hl-mode)
  :init (setq diff-hl-draw-borders nil)
  :config
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)
  ;; Set fringe style
  (setq-default fringes-outside-margins t)

  (with-no-warnings
    (defun my-diff-hl-fringe-bmp-function (_type _pos)
      "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
      (define-fringe-bitmap 'my-diff-hl-bmp
        (vector (if sys/macp #b11100000 #b11111100))
        1 8
        '(center t)))
    (setq diff-hl-fringe-bmp-function #'my-diff-hl-fringe-bmp-function)

    (unless (display-graphic-p)
      (setq diff-hl-margin-symbols-alist
            '((insert . " ") (delete . " ") (change . " ")
              (unknown . " ") (ignored . " ")))
      ;; Fall back to the display margin since the fringe is unavailable in tty
      (diff-hl-margin-mode 1)
      ;; Avoid restoring `diff-hl-margin-mode'
      (with-eval-after-load 'desktop
        (add-to-list 'desktop-minor-mode-table
                     '(diff-hl-margin-mode nil))))

    ;; Integration with magit
    (with-eval-after-load 'magit
      (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
      (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))))

;; [goggles] Highlight modified region
(use-package goggles
  :hook ((prog-mode conf-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse nil))

(provide 'init-highlight)
;;; init-highlight.el ends here
