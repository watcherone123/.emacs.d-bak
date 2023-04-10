;; init-lsp.el --- Initialize lsp (Language Server Protocol) configurations. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Languages Server Protocol(LSP)
;;
;;; Code:

(require 'init-constants)

(use-package eglot
  ;; :disabled
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l r" . eglot-rename)
              ("C-c l f" . eglot-format)
              ("C-c l d" . eldoc))
  :commands (eglot-ensure
             my/rust-expand-macro
             eglot-find-implementation
             eldoc-doc-buffer
             eglot-rename
             eglot-code-action-quickfix)
  :config
  (setq eglot-stay-out-of '(company))
  (setq eglot-events-buffer-size 1)
  (setq completion-category-defaults nil)
  (progn
    (setq eldoc-echo-area-use-multiline-p 3
          eldoc-echo-area-display-truncation-message nil)
    (set-face-attribute 'eglot-highlight-symbol-face nil
                        :background "#b3d7ff")
    (defun my/rust-expand-macro ()
      "Expand macro at point, same as `lsp-rust-analyzer-expand-macro'.
https://rust-analyzer.github.io/manual.html#expand-macro-recursively"
      (interactive)
      (jsonrpc-async-request
       (eglot--current-server-or-lose)
       :rust-analyzer/expandMacro (eglot--TextDocumentPositionParams)
       :error-fn (lambda (msg) (error "Macro expand failed, msg:%s." msg))
       :success-fn
       (lambda (expanded-macro)
	     (cl-destructuring-bind (name format expansion result) expanded-macro
	       (let* ((pr (eglot--current-project))
			      (buf (get-buffer-create (format "*rust macro expansion %s*" (project-root pr)))))
		     (with-current-buffer buf
		       (let ((inhibit-read-only t))
			     (erase-buffer)
			     (insert result)
			     (rust-mode)))
		     (switch-to-buffer-other-window buf))))))
    ))

(use-package lsp-bridge
  :disabled
  :ensure nil
  :config
  ;; (setq lsp-bridge-enable-log t)
  (use-package acm
    :ensure nil
    :bind (:map acm-mode-map
            ("<tab>" . acm-select-next)
            ("<s-tab>" . acm-select-prev)
            ("<backtab>" . acm-select-prev)
            ("C-p" . acm-select-prev)
            ("C-n" . acm-select-next))
    :config
    (if (image-type-available-p 'svg)
        (setq acm-enable-icon t)
      (setq acm-enable-icon nil))
    (setq corfu-excluded-modes '(rust-mode rustic-mode)))
  ;; Enable lsp-bridge.
  (dolist (hook (list
                 'rust-mode-hook
                 'rustic-mode-hook
                 'go-mode-hook))
    (add-hook hook (lambda ()
                     (lsp-bridge-mode 1))))
  ;; For Xref support
  (add-hook 'lsp-bridge-mode-hook (lambda ()
                                    (add-hook 'xref-backend-functions #'lsp-bridge-xref-backend nil t)))
  (general-evil-define-key 'normal lsp-bridge-mode-map
    "ga" 'xref-find-apropos
    "gd" 'lsp-bridge-find-def
    "gD" 'lsp-bridge-return-from-def
    "K"  'lsp-bridge-lookup-documentation
    "gi" 'lsp-bridge-find-impl
    "gr" 'lsp-bridge-find-references))

(provide 'init-lsp)
;;; init-lsp.el ends here
