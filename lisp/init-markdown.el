;;; init-markdown.el --- custom markdown mode. -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(use-package markdown-mode
  :ensure t
  :mode (("\\.mmark\\'" . markdown-mode)
         ("README\\.md$'" . gfm-mode)
         ("\\.md$'" . markdon-mode)
         ("\\.markdown$'" . markdown-mode))
  :init
  (when (executable-find "multimarkdown")
    (setq markdown-command "multimarkdown"))
  ;; Redefine the `auto-mode-alist' entries provided by
  ;; `markdown-mode', because `markdown-mode' adds them to the end of
  ;; the list, and in Emacs 26 an earlier entry takes precedence to
  ;; cause files named "CHANGELOG.md" to open in ChangeLog mode
  ;; instead of Markdown mode.
  ;; issue https://github.com/jrblevin/markdown-mode/issues/331
  (dolist (regex '("\\.md\\'" "\\.markdown\\'"))
    (setq auto-mode-alist
          (cl-remove regex auto-mode-alist :test #'equal :key #'car))
    (add-to-list 'auto-mode-alist `(,regex . markdown-mode)))
  :config
  (general-define-key
    :states 'normal
    :keymaps 'markdown-mode-map
    "<tab>" 'markdown-cycle
    "TAB" 'markdown-cycle))

(provide 'init-markdown)
;;; init-markdown.el ends here
