;; init-base.el --- Better default configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'init-constants)
(require 'init-funcs)

;; Personal information
(setq user-full-name "Sniper"
      user-mail-address shadow-mail-address)

(if sys/gui
  ;; Supress GUI features
  (setq use-file-dialog nil
        use-dialog-box nil
        inhibit-default-init t
        ;; Skip startup screen.
        inhibit-startup-screen t
        inhibit-startup-message t
        inhibit-startup-buffer-menu t))

(if (and sys/gui sys/win32p)
    (progn
    ;;设置窗口位置为屏库左上角(0,0)
    ;; (set-frame-position (selected-frame) 0 0)
    ;; 设置宽和高
    (set-frame-width (selected-frame) 150)
    (set-frame-height (selected-frame) 45)))

(with-no-warnings
  ;; Don't ping things that look like domain names.
  (setq ffap-machine-p-known 'reject)
  ;; Increase how much is read from processes in a single chunk (default is 4kb)
  (setq read-process-output-max #x10000))  ; 64kb

;; Garbage Collector Magic Hack
(use-package gcmh
  :diminish
  :hook (emacs-startup . gcmh-mode)
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold #x1000000)) ; 16MB

;; Optimize for very long lines
(setq bidi-paragraph-direction 'left-to-right
      bidi-inhibit-bpa t)

;; We don't share the file-system with anyone else.
;; No lock files
(setq create-lockfiles nil)

;; Start with a blank canvas.
;; (setq initial-scratch-message "")

;; Warns when opening files bigger than 10MB.
(setq large-file-warning-threshold (* 10 1024 1024))

;; Always load the newest file
;; Load the newer .elc or .el file, rather than stopping at .elc.
(setq load-prefer-newer t)

;; Too useful to disable
(put 'narrow-to-region 'disabled nil)

;; Newline at end of file.
(setq require-final-newline t)

;; Disables the annoying bell ring.
(setq ring-bell-function 'ignore)

;; Number backup files.
(setq version-control t)

;; Make backup files even when in version controlled directory.
(setq vc-make-backup-files t)

;; y is shorter than yes.
(if (boundp use-short-answers)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))

(setq visible-bell t
      delete-by-moving-to-trash t       ; Deleting files go to OS's trash folder
      uniquify-buffer-name-style 'post-forward-angle-brackets ; Show path if names are same
      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil)

;; 复制粘贴
;; Cutting and pasting use primary/clipboard
(setq select-enable-primary t
      select-enable-clipboard t)

;; Deletes excess backup versions silently.
(setq delete-old-versions t)

;; Encoding
;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from Emacs (especially on Microsoft Windows).
(prefer-coding-system 'utf-8)
(setq system-time-locale "C")

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; (set-clipboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
;; (set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(set-file-name-coding-system 'utf-8)
;; (setq locale-coding-system 'utf-8
;;       default-process-coding-system '(utf-8 . utf-8))

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(setq-default major-mode 'text-mode
              indent-tabs-mode nil ;; do not insert tab indentation 没有制表符
              tab-width 4 ;; 将TAB显示为4个空格.
              tab-always-indent 'complete
              fill-column 80 ;; 设置列宽度 行宽
              buffers-menu-max-size 30
              case-fold-search t
              compilation-scroll-output t
              grep-highlight-matches t
              grep-scroll-output t
              line-spacing 0
              ;; 让光标无法离开视线
              mouse-yank-at-point nil
              set-mark-command-repeat-pop t
              tooltip-delay 1.5
              truncate-partial-width-windows nil
              truncate-lines nil           ; Do not display continuation lines
              split-height-threshold nil   ; Disable vertical window splitting
              split-width-threshold nil    ; Disable horizontal window splitting
              )

;; Reverts buffers automatically when underlying files are changed externally.
(use-package autorevert
  :diminish auto-revert-mode
  :ensure nil ; built-in package
  :hook (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-interval 3)
  (auto-revert-avoid-polling t)
  (auto-revert-verbose nil)
  (auto-revert-remote-files t)
  (auto-revert-check-vc-info t)
  (global-auto-revert-non-file-buffers t))

(use-package deferred
  :defer t)

;; Recently opened files
(use-package recentf
  :ensure nil
  :bind (("C-x C-r" . recentf-open-files))
  :defines no-littering-etc-directory no-littering-var-directory
  :after no-littering
  :hook ((after-init . recentf-mode)
         (focus-out-hook . (recentf-save-list recentf-cleanup)))
  :custom
  (recentf-max-saved-items 300)
  (recentf-auto-cleanup 'never)
  (recentf-exclude `(,(expand-file-name package-user-dir)
                     ,no-littering-var-directory
                     ,no-littering-etc-directory
                     ".cache"
                     "cache"
                     "^/tmp/"
                     "/ssh:"
                     "/su\\(do\\)?:"
                     "^/usr/include/"
                     "~$" "\\.log$"
                     "COMMIT_EDITMSG\\'")))

;; Save point position between sessions.
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :config
  ;; Allow commands in minibuffers, will affect `dired-do-dired-do-find-regexp-and-replace' command:
  (setq enable-recursive-minibuffers t)
  (setq savehist-save-minibuffer-history t)
  (setq history-delete-duplicates t)
  (setq history-length 1000)
  (setq savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history))
  (setq savehist-autosave-interval 300))

;; Show line/column number
(use-package simple
  :ensure nil
  :hook (after-init . size-indication-mode) ; 显示百分比进度
  :init
  (setq column-number-mode t   ;; Displays column number in the mode line.
        line-number-mode t     ;; mode-line Line
        ;; kill-whole-line t               ; Kill line including '\n'
        line-move-visual nil
        track-eol t                     ; Keep cursor at end of lines. Require line-move-visual is nil.
        ;; confusing if no fringes
        visual-line-fringe-indicators '(nil right-curly-arrow)
        ;; column starts from 1
        column-number-indicator-zero-based nil
        ;; save current clipboard text
        save-interprogram-paste-before-kill t
        ;; eliminate duplicates
        kill-do-not-save-duplicates t
        ;; show the name of character in `what-cursor-position'
        what-cursor-show-names t
        set-mark-command-repeat-pop t)  ; Repeating C-SPC after popping mark pops it again
  )

(use-package newcomment
  :ensure nil
  :bind ([remap comment-dwim] . #'comment-or-uncomment)
  :config
  (add-hook 'asm-mode-hook
    (lambda ()
      ;; Preferred comment style
      (setq comment-start "// "
            comment-end "")))
  (defun comment-or-uncomment (arg)
    "Comment or uncomment the current line or region.
    If the prefix ARG is specified, call `comment-indent' on the
    current line.
    Else, if the region is active and `transient-mark-mode' is on,
    call `comment-or-uncomment-region'.
    Else, if the current line is empty, insert a comment and indent
    it.
    Else, call `comment-or-uncomment-region' on the current line."
    (interactive "*P")
    (if arg (comment-indent)
      (if (region-active-p)
          (comment-or-uncomment-region (region-beginning) (region-end))
        (if (save-excursion
              (beginning-of-line)
              (looking-at "\\s-*$"))
            (comment-dwim nil)
          (comment-or-uncomment-region (line-beginning-position) (line-end-position))))))
  :custom
  ;; `auto-fill' inside comments.
  ;;
  ;; The quoted text in `message-mode' are identified as comments, so only
  ;; quoted text can be `auto-fill'ed.
  (comment-auto-fill-only-comments t))

;; Workaround with minified source files
(use-package so-long
  :ensure nil
  :when (>= emacs-major-version 27)
  :hook (after-init . global-so-long-mode))

(use-package emacs
  :config
  (setq-default cursor-type 'box)
  ;; (setq-default cursor-in-non-selected-windows '(bar . 2))
  (setq-default blink-cursor-blinks 50)
  (setq-default blink-cursor-interval 0.75)
  (setq-default blink-cursor-delay 0.2)

  ;; (setq mouse-drag-copy-region t)
  (setq make-pointer-invisible t)
  (setq mouse-wheel-follow-mouse t)
  :hook
  (after-init-hook . blink-cursor-mode)
  (after-init-hook . mouse-wheel-mode))

;; mouse wheel optimization
(use-package mwheel
  :ensure nil
  :defer t
  :custom
  ;; (setq mouse-wheel-progressive-speed t)
  (mouse-wheel-progressive-speed nil)
  ;; Mouse & Smooth Scroll
  ;; Scroll one line at a time (less "jumpy" than defaults)
  (when (display-graphic-p)
    (setq mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
          mouse-wheel-scroll-amount-horizontal 1
          mouse-wheel-progressive-speed nil)))

;; Keep ~/.emacs.d clean
(use-package no-littering)

(maple/add-hook 'after-init-hook
  ;; Make escape more nature
  (use-package minibuffer
    :ensure nil
    :defer t
    :bind (:map minibuffer-local-map
          ([escape] . abort-recursive-edit)
          :map minibuffer-local-ns-map
          ([escape] . abort-recursive-edit)
          :map minibuffer-local-completion-map
          ([escape] . abort-recursive-edit)
          :map minibuffer-local-must-match-map
          ([escape] . abort-recursive-edit)
          :map minibuffer-local-isearch-map
          ([escape] . abort-recursive-edit))
    :custom
    (read-file-name-completion-ignore-case t)
    (read-buffer-completion-ignore-case t)
    (completion-ignore-case t)
    (enable-recursive-minibuffers t)
    :init (minibuffer-depth-indicate-mode))

  (use-package transient
    :ensure nil
    :config
    (transient-define-prefix transient-dired-menu ()
      "Dired menu."
      :transient-non-suffix 'transient--do-warn
      [[("(" "details" dired-hide-details-mode)
      (")" "omit-mode" dired-omit-mode)
      ("+" "mkdir" dired-create-directory)
      ("A" "find-regexp" dired-do-find-regexp)
      ("C" "copy" dired-do-copy)        ;; Copy all marked files
      ("D" "delete" dired-do-delete)
      ("E" "mark-extension" dired-mark-extension)]
      [("F" "find-marked-files" dired-do-find-marked-files)
      ("G" "chgrp" dired-do-chgrp)
      ("C-i" "insert-subtree" dired-maybe-insert-subdir)
      ("C-r" "remove subdir" dired-kill-subdir)
      ("i" "insert-subtree" dired-subtree-insert)
      ("r" "remove subdir" dired-subtree-remove)
      ("l" "redisplay" dired-do-redisplay)]   ;; relist the marked or singel directory
      [("O" "display-file" dired-display-file)
      ("o" "open file"dired-find-file-other-window)
      ("Q" "repl regexp" dired-do-find-regexp-and-replace)
      ("R" "rename" dired-do-rename)
      ("S" "symlink" dired-do-symlink)
      ("s" "sort" dired-sort-toggle-or-edit)
      ("M" "chmod" dired-do-chmod)]
      [("t" "toggle" dired-toggle-marks)
      ("m" "mark" dired-mark)
      ("U" "unmark all" dired-unmark-all-marks)
      ("u" "unmark" dired-unmark)
      ("v" "view" dired-view-file)      ;; q to exit, s to search, = gets line #
      ("Y" "rel symlink"dired-do-relsymlink)
      ("Z" "compress" dired-do-compress)]])))

(use-package xref
  :ensure nil
  :after general
  :config
  (general-evil-define-key 'normal xref--xref-buffer-mode-map
    "n" 'xref-next-line
    "p" 'xref-prev-line
    "N" 'xref-next-group
    "P" 'xref-prev-group
    "RET" 'xref-goto-xref
    "q" 'quit-window)
  (setq xref-search-program 'ripgrep
        xref-show-definitions-function #'xref-show-definitions-completing-read
        xref-show-xrefs-function #'xref-show-definitions-completing-read))

(use-package winner
  :ensure nil
  :hook (after-init . winner-mode))

;; [visual-line-mode] Soft line-wrapping
(use-package visual-line-mode
  :ensure nil
  :hook (text-mode . visual-line-mode))

;; Server mode.
;; Use emacsclient to connect
(use-package server
  :ensure nil
  :if sniper-server
  :when (display-graphic-p)
  :defer 1
  :config
  (unless (server-running-p)
    (server-start)))

(provide 'init-base)
;;; init-base.el ends here
