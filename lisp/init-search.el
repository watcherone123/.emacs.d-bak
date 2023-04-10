;; init-search.el --- Initialize search.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; https://github.com/dajva/rg.el
;; doc https://rgel.readthedocs.io
;; -g '!elpa' 不包含elpa
;; -g '*.el' 包含 el文件

;; map "e" 'wgrep-change-to-wgrep-mode
(use-package rg
  :hook (after-init . rg-enable-default-bindings)
  :functions (shadow-custumize-rg
              shadow-custumize-rg-dwim
              shadow-rg-dwim-current-dir)
  :config
  ;; (setq rg-command-line-flags '("-z" "--pcre2"))
  ;; (setq rg-group-result nil);bug enable-wgrep-when-entry-insert
  (setq rg-group-result t)
  (setq rg-hide-command t)
  (setq rg-show-columns nil)
  (setq rg-show-header t)
  (setq rg-custom-type-aliases nil)
  (setq rg-default-alias-fallback "all")
  ;; 解决rg搜索中文乱码
  (modify-coding-system-alist 'process "rg" '(utf-8 . gbk-dos))
  ;; (evil-define-key '(normal visual operator motion emacs) 'global (kbd "<SPC>g") rg-global-map)
  (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases)
  ;; used in rg result buffer
  (rg-define-toggle "--context 3" (kbd "C-c c t"))
  ;; c 语言文件（*.c *.h）中搜索
  (rg-define-toggle "-t c" (kbd "C-c c c"))

  (rg-define-search shadow-custumize-rg
    :query ask
    :format regexp
    :files "everything"
    :dir project
    :confirm prefix
    :flags ("--hidden -g !.git"))

  (defun prot/rg-save-search-as-name ()
    "Save `rg' buffer, naming it after the current search query.
   This function is meant to be mapped to a key in `rg-mode-map'."
    (interactive)
    (let ((pattern (car rg-pattern-history)))
      (rg-save-search-as-name (concat "«" pattern "»"))))

  ;; original `rg-dwim-current-dir' only match current kind of file. But
  ;; I need everything.
  (rg-define-search shadow-rg-dwim-current-dir
    "Search for thing at point in every files under the current
  directory."
    :query ask
    :format literal
    :files "everything"
    :dir current)

  (defun shadow-rg-rerun-filter-by-file ()
    "Rerun last search but exclude selected filename or diredctory with flag: --glob='!*name*'"
    (interactive)
    (let ((flags (rg-search-flags rg-cur-search))
          (dir (read-string (format "%s(file or dir): " (if current-prefix-arg "include" "exclude")))))
      (setq flags (append flags (list (format "--glob='%s*%s*'" (if current-prefix-arg "" "!") dir))))
      (setf (rg-search-flags rg-cur-search) flags)
      (rg-rerun)))

  (defalias 'rg-kill 'kill-compilation)
  ;; Add to existing sub group
  (rg-menu-transient-insert "Manage" "K" "kill-rg" 'rg-kill)

  ;; rip-grep automatically switch to results buffer
  ;; https://github.com/dajva/rg.el/issues/142
  (with-eval-after-load 'rg
    (advice-add 'rg-run :after
                #'(lambda (_pattern _files _dir &optional _literal _confirm _flags)
                    (pop-to-buffer (rg-buffer-name)))))

  ;; c toggle case
  (defun shadow-rg-hook()
    (setq-local scroll-conservatively 101)
    (setq-local scroll-margin 0)
    (setq-local compilation-scroll-output 'first-error)
    (setq-local compilation-always-kill t)
    (evil-define-key '(normal visual operator motion emacs) 'local "x" 'shadow-rg-rerun-filter-by-file)
    (evil-define-key '(normal visual operator motion emacs) 'local "e" 'wgrep-change-to-wgrep-mode)
    (evil-define-key '(normal visual operator motion emacs) 'local "K" 'rg-kill)
    (next-error-follow-minor-mode 0)
    (wgrep-rg-setup)
    ;; (switch-to-buffer-other-window "*rg*")
    (define-key rg-mode-map (kbd "TAB") 'next-error-no-select)
    (define-key rg-mode-map (kbd "<tab>") 'next-error-no-select)
    (define-key rg-mode-map (kbd "<backtab>") 'previous-error-no-select))
  (add-hook 'rg-mode-hook #'shadow-rg-hook))

(provide 'init-search)
;;; init-search.el ends here
