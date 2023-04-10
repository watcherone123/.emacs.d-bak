;; init-ui.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; 设置透明
(set-frame-parameter nil 'alpha 0.9)

;; Title
;; Sets a more useful frame title, showing either a file or a buffer
;; name (if the buffer isn't visiting a file).
(setq frame-title-format '("" invocation-name " - "
                          (:eval (if (buffer-file-name)
                                      (abbreviate-file-name (buffer-file-name))
                                      "%b"))))
(setq icon-title-format frame-title-format)

;; Control over modes displayed in the modeline.
(use-package diminish)

;; config built-in "display-line-numbers-mode" (require Emacs >= 26)
(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode text-mode) . display-line-numbers-mode)
  :init
  (setq-default display-line-numbers-width 2)
  ;; (setq-default display-line-numbers-type 'relative)
  (setq display-line-numbers-current-absolute t))

(use-package all-the-icons
  :if (display-graphic-p)
  :defer t)

(use-package imenu-list
  :defer t
  :ensure nil
  :load-path "site-lisp/imenu-list"
  :commands (imenu-list-smart-toggle)
  :config
  (setq imenu-list-auto-resize t)
  (with-eval-after-load 'evil
    (evil-define-key 'normal imenu-list-major-mode-map (kbd "<tab>") 'hs-toggle-hiding)
    (evil-define-key 'normal imenu-list-major-mode-map (kbd "TAB") 'hs-toggle-hiding)
    (evil-define-key 'normal imenu-list-major-mode-map (kbd "RET") 'imenu-list-ret-dwim)
    (evil-define-key 'normal imenu-list-major-mode-map (kbd "t") 'hs-toggle-hiding)
    (evil-define-key 'normal imenu-list-major-mode-map (kbd "r") 'imenu-list-refresh)
    (evil-define-key 'normal imenu-list-major-mode-map (kbd "q") 'imenu-list-quit-window)))

(use-package centaur-tabs
  :defer t
  :ensure t
  :hook (after-init . centaur-tabs-mode)
  (maple-imenu-mode . centaur-tabs-local-mode)
  (dired-mode . centaur-tabs-local-mode)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  :config
  (setq centaur-tabs-cycle-scope 'tabs) ; Navigate through visible tabs only.
  (setq centaur-tabs-style "chamfer")
  (setq centaur-tabs-height 20)
  (setq centaur-tabs-set-bar 'under)
  ;; (setq centaur-tabs-set-icons t)
  ;; Note: If you're not using Spacmeacs, in order for the underline to display
  ;; correctly you must add the following line:
  (setq x-underline-at-descent-line t))

;; 满行指示器
(if emacs/>=27p
  (use-package display-fill-column-indicator
    :ensure nil
    :hook (prog-mode . display-fill-column-indicator-mode)
    :init
    (setq-default display-fill-column-indicator-character ?\|)))

(use-package sniper-modeline
  :ensure nil; local package
  :load-path "site-lisp/mood-line"
  :hook (after-init . sniper-modeline-mode)
  :config
  (setq sniper-modeline-file-name-max 20))

(provide 'init-ui)
;;; init-ui.el ends here
