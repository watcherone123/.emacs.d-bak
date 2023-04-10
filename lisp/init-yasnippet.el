;; init-yasnippet.el --- Initialize yasnippet configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package yasnippet
  :diminish yas-minor-mode
  :commands (yas-hippie-try-expand)
  :hook (after-init . yas-global-mode)
  :bind (("C-c y i" . yas-insert-snippet)
         ("C-c y x" . yas-expand)
         ("C-c y f" . yas-visit-snippet-file)
         ("C-c y n" . yas-new-snippet)
         ("C-c y t" . yas-tryout-snippet)
         ("C-c y l" . yas-describe-tables)
         ("C-c y g" . yas-global-mode)
         ("C-c y m" . yas-minor-mode)
         ("C-c y r" . yas-reload-all)
         :map yas-keymap
         ("C-i" . yas-next-field-or-maybe-expand))
  :init
  (setq yas-verbosity 1)
  (setq yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  (push 'yas-hippie-try-expand hippie-expand-try-functions-list)
  ;; ;; Disable default yas minor mode map and use hippie integration.
  ;; (setq yas-minor-mode-map (make-sparse-keymap))
  (setq yas-wrap-around-region t))

(use-package yasnippet-snippets :defer t)

(use-package consult-yasnippet
  :after yasnippet
  :bind ("M-s y" . consult-yasnippet))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
