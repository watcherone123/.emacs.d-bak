;; init-company.el --- Initialize company configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package company
  :ensure t
  :diminish company-mode "ⓒ"
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :commands company-cancel
  :bind (("M-\\" . company-complete)
        :map company-mode-map
        ("<backtab>" . company-yasnippet)
        :map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("<tab>" . company-complete-common-or-cycle)
        ("<backtab>" . my-company-yasnippet)
        ("C-g" . company-cancel))
  :hook (after-init . global-company-mode)
  :init
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t
        company-tooltip-limit 12
        company-idle-delay 0
        company-echo-delay (if (display-graphic-p) nil 0)
        company-minimum-prefix-length 1
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-global-modes '(not erc-mode message-mode help-mode
                                   gud-mode eshell-mode shell-mode)
        company-backends '((company-capf :with company-yasnippet)
                           (company-dabbrev-code company-keywords company-files)
                           company-dabbrev))
  (defun my-company-yasnippet ()
    "Hide the current completeions and show snippets."
    (interactive)
    (company-cancel)
    (call-interactively 'company-yasnippet))
  (add-hook 'company-completion-started-hook
            (lambda (&rest ignore)
              (when (and (bound-and-true-p evil-mode) (evil-insert-state-p))
                (define-key evil-insert-state-map (kbd "C-n") nil)
                (define-key evil-insert-state-map (kbd "C-p") nil))))
  :config
  ;; (setq tab-always-indent 'complete)
  (setq company-show-numbers t)
  ;; make previous/next selection in the popup cycles
  ;; (setq company-selection-wrap-around t)
  ;; (setq company-transformers '(company-sort-by-occurrence))

  ;; `yasnippet' integration
  (with-no-warnings
    (defun company-backend-with-yas (backend)
      "Add `yasnippet' to company backend."
      (if (and (listp backend) (member 'company-yasnippet backend))
          backend
        (append (if (consp backend) backend (list backend))
                '(:with company-yasnippet))))

    (defun my-company-enbale-yas (&rest _)
      "Enable `yasnippet' in `company'."
      (setq company-backends (mapcar #'company-backend-with-yas company-backends)))
    ;; Enable in current backends
    (my-company-enbale-yas)

  (use-package company-box
    :hook (c-mode-common . company-box-mode)
    :config
    (setq company-box-backends-colors nil
          company-box-show-single-candidate t
          company-box-max-candidates 50
          company-box-doc-delay 0.5
          company-box-enable-icon nil))))

(provide 'init-company)
;;; init-company.el ends here
