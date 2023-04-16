;; init-evil.el --- Initialize evil-mode configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'init-constants)

(use-package evil
  :hook (after-init . evil-mode)
  :init
  (setq evil-magic t
        evil-echo-state t
        evil-default-state 'normal
        ;; 使能C-u 往上翻
        evil-want-C-u-scroll t
        evil-want-Y-yank-to-eol t
        evil-want-integration t
        evil-want-keybinding nil
        evil-want-visual-char-semi-exclusive t
        evil-indent-convert-tabs t
        evil-ex-search-vim-style-regexp t
        evil-ex-substitute-global t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-insert-skip-empty-lines t
        ;; more vim-like behavior
        evil-symbol-word-search t
        ;; don't activate mark on shift-click
        shift-select-mode nil
        evil-cross-lines t
        evil-move-cursor-back t ;; move back the cursor one position when exiting insert mode
        ;; Prevents esc-key from translating to meta-key in terminal mode.
        evil-esc-delay 0.01
        ;; It's better that the default value is too small than too big.
        evil-shift-width 2
        ;; Controls position of the mode line tag for the current mode,
        ;; e.g. <N>, <I>, etc.  Before places it before the major-mode.
        evil-mode-line-format 'after)
  ;; evil cursor color
  (setq evil-default-cursor '("red" box)
        evil-normal-state-cursor '("DarkGoldenrod2" box)
        evil-insert-state-cursor '("chartreuse3" (bar . 2))
        evil-emacs-state-cursor '("SkyBlue2" box)
        evil-hybrid-state-cursor '("SkyBlue2" (bar . 2))
        evil-replace-state-cursor '("chocolate" (hbar . 2))
        evil-evilified-state-cursor '("LightGoldenrod3" box)
        evil-visual-state-cursor '("gray" (hbar . 2))
        evil-motion-state-cursor '("plum3" box)
        evil-lisp-state-cursor '("HotPink1" box)
        evil-iedit-state-cursor '("firebrick1" box)
        evil-iedit-state-cursor-insert '("firebrick1" (bar . 2)))
  ;; Must be set before evil is loaded.
  ;; (setq evil-respect-visual-line-mode t)
  :config
  (evil-set-undo-system 'undo-redo)
  ;; http://emacs.stackexchange.com/questions/14940
  (fset 'evil-visual-update-x-selection 'ignore)

  ;; Major modes that should default to an insert state.
  (add-to-list 'evil-insert-state-modes 'git-commit-mode)

  ;; evil normal state rg-mode
  ;; (evil-set-initial-state 'rg-mode 'normal)

  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)

  (define-key evil-insert-state-map (kbd "C-a") 'back-to-indentation)
  (define-key evil-insert-state-map (kbd "C-e") 'evil-end-of-line)
  (define-key evil-motion-state-map (kbd "C-e") 'evil-end-of-line)
  ;; evil insert state keybinds
  (define-key evil-insert-state-map (kbd "C-d") 'evil-delete-char)
  (define-key evil-insert-state-map (kbd "C-k") 'kill-line)
  (define-key evil-insert-state-map (kbd "C-p") 'evil-previous-visual-line)
  (define-key evil-insert-state-map (kbd "C-n") 'evil-next-visual-line)
  ;; 系统剪贴板快捷键（C-c复制，C-v粘贴）
  (define-key evil-insert-state-map (kbd "C-v") 'clipboard-yank)
  ;; evil normal state keybinds
  (define-key evil-normal-state-map "Y" (kbd "y$"))
  (define-key evil-normal-state-map (kbd ",q") 'evil-quit)
  (define-key evil-normal-state-map (kbd "C-p") 'open-previous-line)
  (define-key evil-normal-state-map (kbd "C-n") 'open-next-line)
  ;; evil visual state keybinds
  (define-key evil-visual-state-map (kbd "C-e") 'end-of-line)
  (define-key evil-visual-state-map (kbd "C-c") 'clipboard-kill-ring-save)

  (define-key evil-normal-state-map (kbd ",w") 'thing-cut-word)
  (define-key evil-normal-state-map (kbd ",s") 'thing-cut-symbol)
  (define-key evil-normal-state-map (kbd ",x") 'thing-cut-sexp)
  (define-key evil-normal-state-map (kbd ",a") 'thing-cut-parentheses)

  (define-key evil-visual-state-map (kbd "<tab>") 'evil-shift-right)
  (define-key evil-visual-state-map (kbd "<backtab>") 'evil-shift-left)
  (if emacs/>=28p
    ;; Make <TAB> use org-cycle when in org buffers
    (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)))

;; https://github.com/emacs-evil/evil-surround
(use-package evil-surround
  :hook (evil-mode . global-evil-surround-mode))

(use-package evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :init
  ;; The list of supported modes is configured by evil-collection-mode-list
  (evil-collection-init 'view)
  (evil-collection-init 'magit)
  (evil-collection-init 'custom)
  (evil-collection-init 'ibuffer)
  (evil-collection-init 'calendar))

;; https://github.com/hlissner/evil-snipe
;; It provides 2-character motions for quickly (and more accurately) jumping around
;; text, compared to evil's built-in f/F/t/T motions, incrementally highlighting
;; candidate targets as you type.
;; s: 2 char forward; S: 2 char backward
;; f: 1 char forward; F: 1 char backward
;; ;and, repeat search
(use-package evil-snipe
  ;; :after evil
  :hook ((evil-mode . evil-snipe-mode)
         (evil-mode . evil-snipe-override-mode))
  :diminish evil-snipe-local-mode
  :config
  (setq evil-snipe-smart-case t
        evil-snipe-scope 'line
        evil-snipe-repeat-scope 'visible
        evil-snipe-char-fold t)
  ;; (evil-snipe-mode +1)
  ;; (evil-snipe-override-mode +1)
  (add-to-list 'evil-snipe-disabled-modes 'Info-mode nil #'eq)
   ;; fix problems with magit buffer
  (add-hook 'rg-mode-hook #'turn-off-evil-snipe-mode)
  (add-hook 'rg-mode-hook #'turn-off-evil-snipe-override-mode)
  (add-hook 'magit-mode-hook #'turn-off-evil-snipe-mode)
  (add-hook 'magit-mode-hook #'turn-off-evil-snipe-override-mode))

(provide 'init-evil)
;;; init-evil.el ends here
