;; init-keybindings.el --- Initialize key bindings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(define-key global-map (kbd "C-a") 'back-to-indentation)

;; used as tmux prefix key
(global-unset-key (kbd "C-q"))

;; 缩放字体
(if sys/win32p
  (progn
    (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
    (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease))
  (progn
    (global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
    (global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)))

;; [Scrolling keybinding]
(defvar +scrolling-lines 5)
(bind-keys*
  ("M-<down>" . (lambda () (interactive) (scroll-other-window +scrolling-lines)))
  ("M-<up>"   . (lambda () (interactive) (scroll-other-window (- +scrolling-lines))))
  ("C-v"      . (lambda () (interactive) (scroll-up +scrolling-lines)))
  ("M-v"      . (lambda () (interactive) (scroll-up (- +scrolling-lines)))))

(use-package bind-key)

(use-package which-key
  :diminish which-key-mode "Ⓚ"
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-idle-delay 0.3
        which-key-compute-remaps t
        which-key-min-display-lines 1
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-sort-uppercase-first nil
        which-key-side-window-max-width 0.33
        which-key-side-window-max-height 0.25
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
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold))

(use-package general
  :config
  (general-create-definer shadow/space-key-define
    :states '(normal visual motion evilified)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

  (general-create-definer shadow/local-key-define
    :states '(normal visual motion evilified)
    :keymaps 'override
    :prefix ","))

(shadow/space-key-define
  "SPC" 'execute-extended-command
  "b" '(:ignore t :wk "Buffer")
  "b b" 'switch-to-buffer
  "b c" '(shadow/new-empty-buffer :wk "new-buffer")
  "b e" 'eval-buffer
  "b d" 'kill-this-buffer
  "b D" '(shadow/kill-other-buffers :wk "kill-other")
  "b i" '(sky-indent-region-or-buffer :wk "indent-buffer")
  "b k" 'kill-buffer
  "b l" 'ibuffer-list-buffers
  "b g" '(shadow/revert-buffer-no-confirm :wk "revert-buffer")
  "b s" 'save-buffer
  "b S" '(shadow/create-scratch-buffer :wk "create-scratch-buffer")
  "c" '(nil :which-key "Comment")
  "c i" '(comment-or-uncomment :wk "comment-lines")
  "c f" '(astyle-buffer :wk "format code")
  "c w" 'thing-copy-word
  "c s" 'thing-copy-symbol
  "c x" 'thing-copy-sexp
  "c a" 'thing-copy-to-line-beginning
  "c e" 'thing-copy-to-line-end
  "r w" 'thing-replace-word
  "r s" 'thing-replace-symbol
  "r x" 'thing-replace-sexp
  "d" '(nil :which-key "Delete")
  "d d" '(my/delete-delimiter-enclosed-text :wk "delete-enclosed-text")
  "d m" '(delete-carrage-returns :wk "delete ^M")
  "d l" '(sniper-delete-blank-line :wk "delete-blank-line")
  "d w" '(my/delete-word :wk "delete-word")
  "e" '(nil :which-key "Errors")
  "e l" 'flycheck-list-errors
  "e n" 'flycheck-next-error
  "e p" 'flycheck-previous-error
  "f" '(nil :which-key "File")
  "f d" 'consult-fd
  "f e" 'rename-this-file
  "f f" 'find-file
  "f r" 'recentf-open-files
  "g" '(:ignore t :which-key "Git")
  "g a" '(shadow/git-add-current-file :wk "add-current-file")
  "g b" 'magit-blame
  "g c" '(shadow/git-checkout-current-file :wk "checkout-current-file")
  "g d" 'magit-diff-buffer-file
  "g i" 'magit-init
  "g l" 'magit-log-buffer-file
  "g L" 'magit-list-repositories
  "g s" 'magit-status
  "g S" 'magit-stage-file
  "g t" '(hydra-git-timemachine/body :wk "git-timemachine")
  "g u" 'magit-unstage-file
  "g v" 'vc-annotate
  "h" '(:ignore t :wk "highlight & help")
  "h a" 'shadow/toggle-invisibles
  "h f" 'helpful-callable
  "h v" 'helpful-variable
  "h k" 'helpful-key
  "h w" 'shadow/toggle-whitespace
  "h s" 'sp-show-all-position-in-ring
  "h p" 'symbol-overlay-put
  "h c" 'symbol-overlay-remove-all
  "j" '(nil :which-key "Jump")
  "j d" 'dired-jump
  "j f" 'beginning-of-defun
  "j m" '(shadow/jump-match-delimiter :wk "goto-match-delimiter")
  "j w" 'avy-goto-word-or-subword-1
  "m" '(nil :which-key "Bookmark")
  "m s" 'bookmark-set
  "m r" 'bookmark-rename
  "m d" 'bookmark-delete
  "m l" 'bookmark-bmenu-list
  "o" '(nil :which-key "Open")
  "o i" '(shadow/open-init-file :wk "open-init")
  "o c" '(open-custom-file :wk "custom-file")
  "p" '(nil :which-key "Project")
  "p SPC" 'find-file-in-project-by-selected
  "p c" 'find-file-in-current-directory
  "p f" 'find-file-in-project
  "q" '(nil :which-key "Quit")
  "q r" 'restart-emacs
  "s" '(nil :which-key "Search")
  "s /" 'completing-ripgrep
  "s a" 'completing-all-swiper
  "s b" 'my/consult-line-backward
  "s r" 'rg-dwim
  "s i" 'shadow-custumize-rg
  "s q" 'shadow-rg-dwim-current-dir
  "t" '(nil :which-key "Toggle")
  "t ;" 'toggle-frame-fullscreen
  "t b" 'toggle-scroll-bar
  "t f" 'dired-sidebar-toggle-sidebar
  "t i" 'imenu-list-smart-toggle
  "t s" 'symbol-overlay-mode
  "t u" 'vundo
  "t t" 'gts-do-translate
  "t g" 'bing-translate-pop
  "t w" 'delete-trailing-whitespace
  "w" '(nil :which-key "Window")
  "w d" 'delete-window
  "w h" 'evil-window-left
  "w l" 'evil-window-right
  "w k" 'evil-window-up
  "w j" 'evil-window-down
  "w o" 'other-window
  "w /" '(shadow/split-window-right-and-focus :wk "split-window-right")
  "w -" '(shadow/split-window-below-and-focus :wk "split-window-below")
  "w D" 'delete-other-windows)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
