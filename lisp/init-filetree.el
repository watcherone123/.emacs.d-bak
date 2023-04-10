;;; init-filetree --- a tree layout file explorer for Emacs,such as treemacs or neotree. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package neotree
  :ensure t
  :commands (neotree-change-root
             neotree-quick-look
             neotree-toggle
             neotree-hide
             neotree-enter)
  :general
  (general-nmap neotree-mode-map
    "?" 'neotree-hydra/body)
  :init
  (setq neo-create-file-auto-open t
        neo-auto-indent-point nil
        ;; neo-autorefresh t
        neo-smart-open t
        neo-mode-line-type 'none
        neo-window-width 28
        neo-show-updir-line nil
        ;; neo-theme (if (display-graphic-p) 'icons 'arrow)
        neo-banner-message nil
        neo-confirm-create-file #'off-p
        neo-confirm-create-directory #'off-p
        neo-show-hidden-files nil
        neo-keymap-style 'concise
        neo-hidden-regexp-list
        '(;; vcs folders
          "^\\.\\(DS_store\\|git\\|gitignore\\|hg\\|svn\\)$"
          ;; compiled files
          "\\.\\(pyc\\|o\\|elc\\|lock\\|css.map\\)$"
          ;; generated files, caches or local pkgs
          "^\\(node_modules\\|.\\(project\\|cask\\|yardoc\\|sass-cache\\)\\)$"
          ;; org-mode folders
          "^\\.\\(sync\\|export\\|attach\\)$"
          "~$" "\\.emacs*"
          ;; ignore bazel file
          "^bazel-*"
          "^#.*#$"))
  :config
  (defhydra neotree-hydra (:color red
                                    :hint nil)
    "
    Actions^^
    ───────^^────────
    [_o_] enter
    [_l_] change root
    [_c_] create
    [_C_] change directory
    [_d_] delete
    [_h_] select up
    [_g_] refresh
    [_r_] rename
    [_th_] toggle hidden file
    [_q_] Quit
    [_RET_] open
    "
    ("RET" neotree-enter :exit t)
    ("TAB" neotree-stretch-toggle :exit t)
    ("o" neotree-enter :exit t)
    ("q" neotree-hide :exit t)
    ("h" neotree-select-up-node :exit t)
    ("l" neotree-change-root :exit t)
    ("c" neotree-create-node :exit t)
    ("C" neotree-copy-node :exit t)
    ("d" neotree-delete-node :exit t)
    ("g" neotree-refresh :exit t)
    ("r" neotree-rename-node :exit t)
    ("th" neotree-hidden-file-toggle :exit t)
    ("?" nil :exit t))
  )

(provide 'init-filetree)
;;; init-filetree ends here
