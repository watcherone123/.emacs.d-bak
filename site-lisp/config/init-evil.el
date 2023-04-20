;;; init-evil.el --- init-evil config -*- lexical-binding: t -*-

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
(setq undo-limit 8000000
      undo-strong-limit 8000000
      undo-outer-limit 8000000)


(add-hook 'after-init-hook (lambda ()
                             (require 'evil)
                             (evil-mode 1)
                             ))

(with-eval-after-load 'evil

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
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
  )

(add-hook 'evil-mode-hook #'(lambda ()
                              (require 'evil-surround)
                              (global-evil-surround-mode)
                              ))

(with-eval-after-load 'evil
  (setq evil-collection-setup-minibuffer t
        evil-collection-init 'view
        evil-collection-init 'magit
        evil-collection-init 'custom
        evil-collection-init 'ibuffer
        evil-collection-init 'calendar
        )
  (require 'evil-collection)
  )

(with-eval-after-load 'evil
; {{ define my own text objects, works on evil v1.0.9 using older method
;; @see http://stackoverflow.com/questions/18102004/emacs-evil-mode-how-to-create-a-new-text-object-to-select-words-with-any-non-sp
(defmacro my-evil-define-and-bind-text-object (key start-regex end-regex)
  (let* ((inner-name (make-symbol "inner-name"))
         (outer-name (make-symbol "outer-name")))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

;; between equal signs
(my-evil-define-and-bind-text-object "=" "=" "=")
;; between pipe characters:
(my-evil-define-and-bind-text-object "|" "|" "|")
;; regular expression
(my-evil-define-and-bind-text-object "/" "/" "/")
;; trimmed line
(my-evil-define-and-bind-text-object "l" "^ *" " *$")
;; angular template
(my-evil-define-and-bind-text-object "r" "\{\{" "\}\}")
;; }}

;; {{ nearby file path as text object,
;;      - "vif" to select base name
;;      - "vaf" to select full path
;;
;;  example:
;;    "/hello/world"
;;    "/test/back.exe"
;;    "C:hello\\hello\\world\\test.exe"
;;    "D:blah\\hello\\world\\base.exe"
(defun my-evil-path-is-separator-char (ch)
  "Check ascii table that CH is slash characters.
If the character before and after CH is space or tab, CH is NOT slash"
  (let* (rlt prefix-ch postfix-ch)
    (when (and (> (point) (point-min)) (< (point) (point-max)))
      (save-excursion
        (backward-char)
        (setq prefix-ch (following-char)))
      (save-excursion
        (forward-char)
        (setq postfix-ch (following-char))))
    (if (and (not (or (= prefix-ch 32) (= postfix-ch 32)))
             (or (= ch 47) (= ch 92)) )
        (setq rlt t))
    rlt))

(defun my-evil-path-not-path-char (ch)
  "Check ascii table for character CH."
  (or (and (<= 0 ch) (<= ch 32))
      (memq ch
            '(34 ; double quotes
              ?'
              40 ; (
              41 ; )
              ?<
              ?>
              91 ; [
              93 ; ]
              ?`
              ?{
              ?}
              127))))

(defun my-evil-path-calculate-path (b e)
  (let* (rlt f)
    (when (and b e)
      (setq b (+ 1 b))
      (when (save-excursion
              (goto-char e)
              (setq f (my-evil-path-search-forward-char 'my-evil-path-is-separator-char t))
              (and f (>= f b)))
        (setq rlt (list b (+ 1 f) (- e 1)))))
    rlt))

(defun my-evil-path-get-path-already-inside ()
  (let* (b e)
    (save-excursion
      (setq b (my-evil-path-search-forward-char 'my-evil-path-not-path-char t)))
    (save-excursion
      (when (setq e (my-evil-path-search-forward-char 'my-evil-path-not-path-char))
        (goto-char (- e 1))
        ;; example: hello/world,
        (if (memq (following-char) '(?, ?.))
            (setq e (- e 1)))))
    (my-evil-path-calculate-path b e)))

(defun my-evil-path-search-forward-char (fn &optional backward)
  (let* (found
         rlt
         (limit (if backward (point-min) (point-max)))
         out-of-loop)
    (save-excursion
      (while (not out-of-loop)
        ;; for the char, exit
        (if (setq found (apply fn (list (following-char))))
            (setq out-of-loop t)
          ;; reach the limit, exit
          (if (= (point) limit)
              (setq out-of-loop t)
            ;; keep moving
            (if backward (backward-char) (forward-char)))))
      (if found (setq rlt (point))))
    rlt))

(defun my-evil-path-extract-region ()
  "Find the closest file path."
  (let* (rlt b f1 f2)
    (if (and (not (my-evil-path-not-path-char (following-char)))
             (setq rlt (my-evil-path-get-path-already-inside)))
        ;; maybe (point) is in the middle of the path
        t
      ;; need search forward AND backward to find the right path
      (save-excursion
        ;; path in backward direction
        (when (setq b (my-evil-path-search-forward-char #'my-evil-path-is-separator-char t))
          (goto-char b)
          (setq f1 (my-evil-path-get-path-already-inside))))
      (save-excursion
        ;; path in forward direction
        (when (setq b (my-evil-path-search-forward-char #'my-evil-path-is-separator-char))
          (goto-char b)
          (setq f2 (my-evil-path-get-path-already-inside))))
      ;; pick one path as the final result
      (cond
       ((and f1 f2)
        (if (> (- (point) (nth 2 f1)) (- (nth 0 f2) (point)))
            (setq rlt f2)
          (setq rlt f1)))
       (f1
        (setq rlt f1))
       (f2
        (setq rlt f2))))

    rlt))

(evil-define-text-object my-evil-path-inner-text-object (&optional count begin end type)
  "File name of nearby path"
  (let* ((selected-region (my-evil-path-extract-region)))
    (if selected-region
        (evil-range (nth 1 selected-region) (nth 2 selected-region) type :expanded t))))

(evil-define-text-object my-evil-path-outer-text-object (&optional count begin end type)
  "Nearby path."
  (let* ((selected-region (my-evil-path-extract-region)))
    (when selected-region
      (evil-range (car selected-region) (+ 1 (nth 2 selected-region)) type :expanded t))))

(define-key evil-inner-text-objects-map "f" 'my-evil-path-inner-text-object)
(define-key evil-outer-text-objects-map "f" 'my-evil-path-outer-text-object)
;; }};; {{ nearby file path as text object,
;;      - "vif" to select base name
;;      - "vaf" to select full path
;;
;;  example:
;;    "/hello/world"
;;    "/test/back.exe"
;;    "C:hello\\hello\\world\\test.exe"
;;    "D:blah\\hello\\world\\base.exe"
(defun my-evil-path-is-separator-char (ch)
  "Check ascii table that CH is slash characters.
If the character before and after CH is space or tab, CH is NOT slash"
  (let* (rlt prefix-ch postfix-ch)
    (when (and (> (point) (point-min)) (< (point) (point-max)))
      (save-excursion
        (backward-char)
        (setq prefix-ch (following-char)))
      (save-excursion
        (forward-char)
        (setq postfix-ch (following-char))))
    (if (and (not (or (= prefix-ch 32) (= postfix-ch 32)))
             (or (= ch 47) (= ch 92)) )
        (setq rlt t))
    rlt))

(defun my-evil-path-not-path-char (ch)
  "Check ascii table for character CH."
  (or (and (<= 0 ch) (<= ch 32))
      (memq ch
            '(34 ; double quotes
              ?'
              40 ; (
              41 ; )
              ?<
              ?>
              91 ; [
              93 ; ]
              ?`
              ?{
              ?}
              127))))

(defun my-evil-path-calculate-path (b e)
  (let* (rlt f)
    (when (and b e)
      (setq b (+ 1 b))
      (when (save-excursion
              (goto-char e)
              (setq f (my-evil-path-search-forward-char 'my-evil-path-is-separator-char t))
              (and f (>= f b)))
        (setq rlt (list b (+ 1 f) (- e 1)))))
    rlt))

(defun my-evil-path-get-path-already-inside ()
  (let* (b e)
    (save-excursion
      (setq b (my-evil-path-search-forward-char 'my-evil-path-not-path-char t)))
    (save-excursion
      (when (setq e (my-evil-path-search-forward-char 'my-evil-path-not-path-char))
        (goto-char (- e 1))
        ;; example: hello/world,
        (if (memq (following-char) '(?, ?.))
            (setq e (- e 1)))))
    (my-evil-path-calculate-path b e)))

(defun my-evil-path-search-forward-char (fn &optional backward)
  (let* (found
         rlt
         (limit (if backward (point-min) (point-max)))
         out-of-loop)
    (save-excursion
      (while (not out-of-loop)
        ;; for the char, exit
        (if (setq found (apply fn (list (following-char))))
            (setq out-of-loop t)
          ;; reach the limit, exit
          (if (= (point) limit)
              (setq out-of-loop t)
            ;; keep moving
            (if backward (backward-char) (forward-char)))))
      (if found (setq rlt (point))))
    rlt))

(defun my-evil-path-extract-region ()
  "Find the closest file path."
  (let* (rlt b f1 f2)
    (if (and (not (my-evil-path-not-path-char (following-char)))
             (setq rlt (my-evil-path-get-path-already-inside)))
        ;; maybe (point) is in the middle of the path
        t
      ;; need search forward AND backward to find the right path
      (save-excursion
        ;; path in backward direction
        (when (setq b (my-evil-path-search-forward-char #'my-evil-path-is-separator-char t))
          (goto-char b)
          (setq f1 (my-evil-path-get-path-already-inside))))
      (save-excursion
        ;; path in forward direction
        (when (setq b (my-evil-path-search-forward-char #'my-evil-path-is-separator-char))
          (goto-char b)
          (setq f2 (my-evil-path-get-path-already-inside))))
      ;; pick one path as the final result
      (cond
       ((and f1 f2)
        (if (> (- (point) (nth 2 f1)) (- (nth 0 f2) (point)))
            (setq rlt f2)
          (setq rlt f1)))
       (f1
        (setq rlt f1))
       (f2
        (setq rlt f2))))

    rlt))

(evil-define-text-object my-evil-path-inner-text-object (&optional count begin end type)
  "File name of nearby path"
  (let* ((selected-region (my-evil-path-extract-region)))
    (if selected-region
        (evil-range (nth 1 selected-region) (nth 2 selected-region) type :expanded t))))

(evil-define-text-object my-evil-path-outer-text-object (&optional count begin end type)
  "Nearby path."
  (let* ((selected-region (my-evil-path-extract-region)))
    (when selected-region
      (evil-range (car selected-region) (+ 1 (nth 2 selected-region)) type :expanded t))))

(define-key evil-inner-text-objects-map "f" 'my-evil-path-inner-text-object)
(define-key evil-outer-text-objects-map "f" 'my-evil-path-outer-text-object)
;; }}

;; {{ paren range text object
(defun my-evil-paren-range (count beg end type inclusive)
  "Get minimum range of paren text object.
COUNT, BEG, END, TYPE is used.  If INCLUSIVE is t, the text object is inclusive.
FN is function to get range."
  (let* ((parens '("()" "[]" "{}" "<>" "\"\"" "''" "``"))
         (pos (point))
         range
         found-range)
    (dolist (p parens)
      (condition-case nil
          (let* ((c1 (aref p 0))
                 (c2 (aref p 1)))
            (setq range (if (eq c1 c2) (evil-select-quote c1 beg end type count inclusive)
                          (evil-select-paren c1 c2 beg end type count inclusive))))
        (error nil))
      (when (and range (<= (nth 0 range) pos) (< pos (nth 1 range)))
        (cond
         (found-range
          (when (< (- (nth 1 range) (nth 0 range))
                   (- (nth 1 found-range) (nth 0 found-range)))
            (setf (nth 0 found-range) (nth 0 range))
            (setf (nth 1 found-range) (nth 1 range))))
         (t
          (setq found-range range)))))
    found-range))

(evil-define-text-object my-evil-a-paren (count &optional beg end type)
  "Select a paren."
  :extend-selection t
  (my-evil-paren-range count beg end type t))

(evil-define-text-object my-evil-inner-paren (count &optional beg end type)
  "Select 'inner' paren."
  :extend-selection nil
  (my-evil-paren-range count beg end type nil))

(define-key evil-inner-text-objects-map "g" #'my-evil-inner-paren)
(define-key evil-outer-text-objects-map "g" #'my-evil-a-paren)
;; }}

(define-key evil-normal-state-map "gh" 'beginning-of-defun)

;; As a general rule, mode specific evil leader keys started
;; with upper cased character or 'g' or special character except "=" and "-"
(evil-declare-key 'normal org-mode-map
  "gh" 'outline-up-heading
  "$" 'org-end-of-line ; smarter behavior on headlines etc.
  "^" 'org-beginning-of-line ; ditto
  "<" (lambda () (interactive) (org-demote-or-promote 1)) ; out-dent
  ">" 'org-demote-or-promote ; indent
  (kbd "TAB") 'org-cycle)

(evil-declare-key 'normal markdown-mode-map
  "gh" 'outline-up-heading
  (kbd "TAB") 'markdown-cycle)

;; {{ specify major mode uses Evil (vim) NORMAL state or EMACS original state.
;; You may delete this setup to use Evil NORMAL state always.
(defvar my-initial-evil-state-setup
  '((minibuffer-inactive-mode . emacs)
    (calendar-mode . emacs)
    (special-mode . emacs)
    (grep-mode . emacs)
    (Info-mode . emacs)
    (term-mode . emacs)
    (log-edit-mode . emacs)
    (vc-log-edit-mode . emacs)
    (magit-log-edit-mode . emacs)
    (erc-mode . emacs)
    (diff-mode . emacs)
    (ffip-diff-mode . normal)
    (neotree-mode . emacs)
    (w3m-mode . emacs)
    (gud-mode . emacs)
    (help-mode . emacs)
    (eshell-mode . emacs)
    (shell-mode . emacs)
    (vterm-mode . emacs)
    (xref--xref-buffer-mode . emacs)
    (epa-key-list-mode . emacs)
    (fundamental-mode . emacs)
    (weibo-timeline-mode . emacs)
    (weibo-post-mode . emacs)
    (woman-mode . emacs)
    (sr-mode . emacs)
    (profiler-report-mode . emacs)
    (dired-mode . emacs)
    (compilation-mode . emacs)
    (speedbar-mode . emacs)
    (ffip-file-mode . emacs)
    (messages-buffer-mode . normal)
    )
  "Default evil state per major mode.")
;; }}

;; I prefer Emacs way after pressing ":" in evil-mode
(define-key evil-ex-completion-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-ex-completion-map (kbd "C-b") 'backward-char)
(define-key evil-ex-completion-map (kbd "M-p") 'previous-complete-history-element)
(define-key evil-ex-completion-map (kbd "M-n") 'next-complete-history-element)

;; initial evil state per major mode
  (dolist (p my-initial-evil-state-setup)
    (evil-set-initial-state (car p) (cdr p)))
)


(with-eval-after-load 'evil
  (require 'evil-snipe)
  (add-hook 'evil-mode-hook #'evil-snipe-mode)
  (add-hook 'evil-mode-hook #'evil-snipe-override-mode)

  (setq evil-snipe-smart-case t
        evil-snipe-scope 'line
        evil-snipe-repeat-scope 'visible
        evil-snipe-char-fold t)
  (add-to-list 'evil-snipe-disabled-modes 'Info-mode nil #'eq)
  ;; fix problems with magit buffer
  (add-hook 'rg-mode-hook #'turn-off-evil-snipe-mode)
  (add-hook 'rg-mode-hook #'turn-off-evil-snipe-override-mode)
  (add-hook 'magit-mode-hook #'turn-off-evil-snipe-mode)
  (add-hook 'magit-mode-hook #'turn-off-evil-snipe-override-mode)
  )

(provide 'init-evil)
