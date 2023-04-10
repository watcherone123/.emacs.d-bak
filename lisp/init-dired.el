;; init-dired.el --- Initialize dired configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'init-funcs)

;; Directory operations
(use-package dired
  :ensure nil ; built-in package
  :config
  (general-evil-define-key 'normal dired-mode-map
    "a" 'dired-find-alternate-file
    "d" 'dired-flag-file-deletion
    "f" 'fd-dired
    "gf" 'dired-find-file
    "gy" 'dired-show-file-type
    "gr" 'revert-buffer
    "h" 'dired-up-directory
    "i" 'dired-toggle-read-only
    "j" 'dired-next-line
    "k" 'dired-previous-line
    "l" 'dired-find-file
    "m" 'dired-mark
    "o" 'dired-sort-toggle-or-edit
    "q" 'quit-window
    "r" 'dired-do-redisplay
    "th" 'dired-omit-mode
    "tt" 'dired-toggle-marks
    "u" 'dired-unmark
    "v" 'dired-git-info-mode
    "x" 'dired-do-flagged-delete
    "RET" 'dired-find-file
    ;; Commands to mark or flag certain categories of files
    "+" 'dired-create-directory
    "^" 'dired-up-directory
    "#" 'dired-flag-auto-save-files
    "." 'dired-clean-directory
    "~" 'dired-flag-backup-files
    "!" 'dired-do-shell-command
    "&" 'dired-do-async-shell-command
    ;; Upper case keys (except !) for operating on the marked files
    "A" 'dired-do-find-regexp
    "C" 'dired-do-copy
    "B" 'dired-do-byte-compile
    "D" 'dired-do-delete
    "G" 'dired-do-chgrp
    "I" 'dired-maybe-insert-subdir
    "J" 'dired-goto-file
    "K" 'dired-do-kill-lines
    "P" 'dired-do-print
    "Q" 'dired-do-find-regexp-and-replace
    "R" 'dired-do-rename
    "S" 'dired-do-symlink
    "T" 'dired-do-touch
    "W" 'browse-url-of-dired-file
    "X" 'dired-do-shell-command
    "Y" 'dired-copy-filename-as-kill
    "Z" 'dired-do-compress
    "?" 'transient-dired-menu)
  (when (or (and sys/macp (executable-find "gls"))
            (and (not sys/macp) (executable-find "ls")))
    ;; Using `insert-directory-program'
    (setq ls-lisp-use-insert-directory-program t)

    ;; Show directory first
    (setq dired-listing-switches "-alh --group-directories-first"))
  (setq dired-dwim-target t)
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  (setq dired-hide-details-hide-symlink-targets nil)
  (put 'dired-find-alternate-file 'disabled nil)

  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory
  ;; automatically refresh dired buffer on changes
  (add-hook 'dired-mode-hook 'auto-revert-mode)

  (use-package dired-x
    :ensure nil ; built-in package
    ))

(use-package fd-dired
  :if *fd*
  :defer t)

;; Turn Dired into a tree browser
(use-package dired-subtree
  :config
  (setq dired-subtree-use-backgrounds nil))

(defun sniper-dired-up-directory()
    "Dired-up-directory."
    (interactive)
    (if (dired-subtree--get-ov)
        (dired-subtree-up)
        (if (> (line-number-at-pos) 5)
            (goto-line 5)
            (dired-sidebar-up-directory))))

(use-package dired-collapse
  :hook (dired-mode . dired-collapse-mode))

(use-package dired-sidebar
  :bind ("C-x C-n" . dired-sidebar-toggle-sidebar)
  (:map dired-sidebar-mode-map
        ("-" . dired-do-hardlink))
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal dired-sidebar-mode-map "h" 'sniper-dired-up-directory)
    (evil-define-key 'normal dired-sidebar-mode-map "q" 'kill-buffer-and-window))
  (unless (display-graphic-p)
    (general-define-key
      :states 'normal
      :keymaps 'dired-sidebar-mode-map
      "C-m" 'dired-sidebar-find-file
      "RET" 'dired-sidebar-find-file
      "<tab>" 'dired-sidebar-subtree-toggle
      "TAB" 'dired-sidebar-subtree-toggle))
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  (setq dired-sidebar-theme 'icons)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

(use-package all-the-icons-dired
  :defer
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package tempbuf
  :demand t
  :ensure nil; local package
  :load-path "site-lisp/tempbuf"
  :config
  (add-hook 'dired-mode-hook 'turn-on-tempbuf-mode))

(provide 'init-dired)
;;; init-dired.el ends here
