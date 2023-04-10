;; init-prog.el --- Initialize flycheck configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; General programming configurations.
;;

;;; Code:

(use-package prog-mode
  :ensure nil
  :hook (prog-mode . prettify-symbols-mode)
  :config
  (setq-default prettify-symbols-alist
                '(("lambda" . ?λ)
                  ("<-" . ?←)
                  ("->" . ?→)
                  ("->>" . ?↠)
                  ("=>" . ?⇒)
                  ("/=" . ?≠)
                  ("!=" . ?≠)
                  ("<=" . ?≤)
                  (">=" . ?≥)
                  ("=<<" . (?= (Br . Bl) ?≪))
                  (">>=" . (?≫ (Br . Bl) ?=))
                  ("<=<" . ?↢)
                  (">=>" . ?↣)))
  (setq prettify-symbols-unprettify-at-point 'right-edge))

;; C/C++ Mode
(use-package cc-mode
  :ensure nil
  :bind (:map c-mode-base-map
              ("C-c c" . compile))
  :hook (c-mode-common . c-common-setup)
  :config
  (defun c-common-setup ()
    (c-set-style "bsd")
    ;; (setq tab-width 4)
    (setq evil-shift-width 4)
    (setq c-basic-offset 4)
    ;; Preferred comment style
    (setq comment-start "// "
          comment-end ""))

  (use-package modern-cpp-font-lock
    :diminish
    :init (modern-c++-font-lock-global-mode t)))

(use-package cmake-mode
  :ensure t
  :mode (
	 ("CMakeLists\\.txt\\'" . cmake-mode)
	 ("\\.cmake\\'" . cmake-mode)))

(use-package hideshow
  :ensure nil
  :hook(prog-mode . hs-minor-mode)
  :config
  ;; 这里额外启用了 :box t 属性使得提示更加明显
  (defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box t))))
  (defun hideshow-folded-overlay-fn (ov)
      (when (eq 'code (overlay-get ov 'hs))
        (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
              (info (format " ... #%d " nlines)))
          (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))
  (setq hs-set-up-overlay 'hideshow-folded-overlay-fn))

(use-package hideif
  :ensure nil
  :after evil
  :hook (c-mode-common . (lambda ()
                           (hide-ifdef-mode)
                           (define-key evil-normal-state-map (kbd "zf") 'hide-ifdefs)
                           (define-key evil-normal-state-map (kbd "zF") 'show-ifdefs)
                           (define-key evil-normal-state-map (kbd "zs") 'hif-show-all))))
(use-package citre
  :defer t
  :bind (("C-x c c" . citre-create-tags-file)
         ("C-x c e" . citre-edit-tags-file-recipe)
         ("C-x c j" . citre-jump)
         ("C-x c J" . citre-jump-back)
         ("C-x c r" . citre-jump-to-reference)
         ("C-x c p" . citre-peek)
         ("C-x c u" . citre-update-this-tags-file))
  :hook (after-init . (lambda ()
                        (require 'citre-config)))
  :init
  (setq citre-enable-imenu-integration nil
        citre-enable-capf-integration nil)
  :config
  (setq
    ;; Set this if you want to always use one location to create a tags file.
    citre-default-create-tags-file-location 'global-cache
    ;; See the "Create tags file" section above to know these options
    citre-use-project-root-when-creating-tags t
    citre-prompt-language-for-ctags-command t
    citre-auto-enable-citre-mode-modes '(prog-mode))
  (setq citre-ctags-cmd-buf-default-cmd
    "ctags -o
    %TAGSFILE%
    --languages=all
    --kinds-all=*
    --fields=*
    --extras=* -R -n")
    )

(use-package astyle
  :ensure t
  :when (executable-find "astyle")
  :config
  (setq astyle-style "otbs"
        astyle-indent 4)
  ;; github.com/MaJerle/c-code-style
  (setq astyle-custom-args
        '("--align-pointer=type"
          "--align-reference=name"
          "--unpad-paren"
          "--indent-classes"
          "--indent-switches"
          "--indent-preproc-define"
          "--indent-col1-comments"
          "--attach-namespaces"
          "--attach-classes"
          "--attach-inlines"
          "--attach-extern-c"
          "--attach-closing-while"
          "--pad-oper"
          "--pad-header"
          "--pad-comma"
          ;; "--break-return-type"
          "--break-one-line-headers"
          "--add-braces"
          "--convert-tabs"
          "--max-code-length=200"
          "--min-conditional-indent=0"
          "--max-continuation-indent=120"
          "--mode=c"
          "--lineend=windows"
          "--recursive"
          "--suffix=none"
          "--preserve-date"
          "--formatted"
          )))

;; rust-mode
(use-package rust-mode
  :mode "\\.rs\\'"
  :hook (rust-mode . my/rust-compile)
  :config
  (setq rust-format-on-save nil)
  (add-hook 'rust-mode-hook 'eglot-ensure)
  (add-hook 'rust-mode-hook (lambda ()
                              (setq evil-shift-width 4)))
  (defun my/rust-compile ()
    (setq-local compile-command "cargo check --color never --tests")))

;; Cargo support
(use-package cargo
  :ensure t
  :hook ((rust-mode . cargo-minor-mode))
  :config
  (defun my/cargo-test-current ()
    (interactive)
    (setenv "RUST_LOG" "debug")
    (cargo-process-current-test))
  :bind (:map rust-mode-map
         (("C-c C-t" . my/cargo-test-current)))
  :custom ((cargo-process--command-current-test "test --color never")
           (cargo-process--enable-rust-backtrace t)
           (cargo-process--command-flags "--  --nocapture")))

(use-package flycheck-rust
  :ensure t
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))



(if (sniper-treesit-available-p)
  (use-package treesit-langs
    :ensure nil
    :load-path "site-lisp/treesit-langs"
    :hook (prog-mode . sniper-ts-mode))
    ;; )

  (maple/add-hook 'after-init-hook
    (use-package tree-sitter
        :ensure t
        :custom-face
        (tree-sitter-hl-face:operator      ((t (:inherit default))))
        ;; (tree-sitter-hl-face:constant        ((t (:foreground "LimeGreen"))))
        ;; c 语言结构体
        (tree-sitter-hl-face:property        ((t (:inherit font-lock-variable-name-face))))
        (tree-sitter-hl-face:function.call ((t (:inherit font-lock-function-name-face
                                                :underline t
                                                :italic t))))
        (tree-sitter-hl-face:function.method.call ((t)))
        (tree-sitter-hl-face:method.call   ((t (:inherit font-lock-function-name-face))))
        :config
        (global-tree-sitter-mode)
        (setq tsc-dyn-get-from '(:github))
        (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

    (use-package tree-sitter-langs
        :ensure t
        :after tree-sitter)
    ))

(use-package lua-mode
  :defer t
  :config
  (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))

(use-package vimrc-mode)

;; Major mode for editing web templates
(use-package web-mode
  :mode "\\.\\(phtml\\|php\\|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

;; [flymake] On-the-fly syntax checker
(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :bind (("C-c f ]" . flymake-goto-next-error)
         ("C-c f [" . flymake-goto-prev-error)
         ("C-c f b" . flymake-show-buffer-diagnostics))
  :config
  (setq
   flymake-diagnostic-functions nil
   ;; Check only on save
   flymake-no-changes-timeout 1)
  )

(use-package dts-mode
  :mode (("\\.dts\\'" . dts-mode)
         ("\\.dtsi\\'" . dts-mode)))

(provide 'init-prog)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
