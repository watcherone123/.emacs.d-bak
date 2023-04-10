;; init-edit.el --- Initialize editing configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'init-constants)

;; Elec pair
(use-package elec-pair
  :defer t
  :ensure nil
  :init (add-hook 'after-init-hook #'electric-pair-mode))

;; Hungry deletion
(use-package hungry-delete
  :defer t
  :ensure t
  :bind (("C-c DEL" . hungry-delete-backward))
  :bind (("C-c d" . hungry-delete-forward))
  :diminish hungry-delete-mode
  :init (add-hook 'after-init-hook #'global-hungry-delete-mode)
  (setq hungry-delete-chars-to-skip " \t\n\r\f\v"))

;; Edit multiple regions in the same way simultaneously
(use-package iedit
  :ensure t
  :bind (("C-;" . iedit-mode)
         ("C-x r RET" . iedit-rectangle-mode)
         :map isearch-mode-map ("C-;" . iedit-mode-from-isearch)
         :map esc-map ("C-;" . iedit-execute-last-modification)
         :map help-map ("C-;" . iedit-mode-toggle-on-function)))

(use-package maple-iedit
  :ensure nil
  :after evil
  :load-path "site-lisp/maple-iedit"
  :commands (maple-iedit-match-all maple-iedit-match-next maple-iedit-match-previous)
  :bind (:map evil-visual-state-map
              ("n" . maple-iedit-menu)
              ("C-n" . maple-iedit-match-next)
              ("C-p" . maple-iedit-match-previous)
              ("C-t" . maple-iedit-skip-and-match-next))
  :config
  (setq maple-iedit-evil-keybind t
        maple-iedit-ignore-case t)
  (transient-define-prefix maple-iedit-menu ()
    "Org Insert template menu"
    :transient-suffix     'transient--do-stay
    :transient-non-suffix 'transient--do-warn
    [[("a" "[all match]" maple-iedit-match-all )]
    [("n" "[next]" maple-iedit-match-next)]
    [("t" "[skip and next]" maple-iedit-skip-and-match-next)]
    [("T" "[skip and previous]" maple-iedit-skip-and-match-previous)]
    [("p" "[prev]" maple-iedit-match-previous)]]))

;; [ialign] Interactive align
(use-package ialign
  :ensure t)

;; Increase selected region by semantic units
(use-package expand-region
  :bind
  ("M-=" . er/expand-region)
  ("M--" . er/contract-region))

(use-package whitespace
  :hook ((prog-mode markdown-mode conf-mode) . enable-trailing-whitespace)
  :config
  ;; Visualize TAB, (HARD) SPACE, NEWLINE
  ;; (setq-default show-trailing-whitespace t) ; Don't show trailing whitespace by default

  (defun shadow/toggle-invisibles ()
    "Toggles the display of indentation and space characters."
    (interactive)
    (if (bound-and-true-p whitespace-mode)
        (whitespace-mode -1)
      (whitespace-mode)))

  (defun shadow/toggle-whitespace ()
    "Toggles the display of indentation and space characters."
    (interactive)
    (if (bound-and-true-p show-trailing-whitespace)
        (disable-trailing-whitespace)
      (enable-trailing-whitespace)))

  (defun enable-trailing-whitespace ()
    "Show trailing spaces and delete on saving."
    (setq show-trailing-whitespace t)
    (add-hook 'before-save-hook #'delete-trailing-whitespace)
    (message "enable-trailing-whitespace"))

  (defun disable-trailing-whitespace ()
    "Show trailing spaces and delete on saving."
    (setq show-trailing-whitespace nil)
    (remove-hook 'before-save-hook #'delete-trailing-whitespace)
    (message "disable-trailing-whitespace")))

(use-package thing-edit
  :ensure nil; local package
  :load-path "site-lisp/thing-edit")

(when use-rime
  ;; https://github.com/DogLooksGood/emacs-rime
  (use-package rime
    :bind
    (:map rime-mode-map
          ("C-`" . 'rime-send-keybinding))
    :config
    (setq rime-user-data-dir "~/.config/rime")
    (setq rime-posframe-properties
          (list :background-color "#333333"
                :foreground-color "#dcdccc"
                :font "WenQuanYi Micro Hei Mono-14"
                :internal-border-width 10))
    (setq default-input-method "rime"
          rime-show-candidate 'posframe))
)

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :ensure nil
  :hook(;; restore window layout when done
        (ediff-quit . winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

(use-package vundo
  :ensure nil; local package
  :load-path "site-lisp/vundo")

(use-package save-position
  :ensure nil; local package
  :bind (("M-p" . sp-push-position-to-ring);;  来保存或者删除一个位置
                  ;;  来跳转不同的位置
         ("M-o" . sp-get-position-from-ring)))

;; Delete selection if you insert
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(provide 'init-edit)
;;; init-edit.el ends here
