;;; init-git.el --- version contral setup. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package magit
  :if *git*
  :commands (magit-status magit-init magit-file-log magit-blame-mode)
  :bind
  (:map magit-mode-map
        ("H" . evil-window-top)
        ("L" . evil-window-down))
  (("C-c g i" . magit-init)
   ("C-c g f" . magit-file-log)
   ("C-c g b" . magit-blame-mode)
   ("C-c g m" . magit-branch-manager)
   ("C-c g c" . magit-branch)
   ("C-c g s" . magit-status)
   ("C-c g r" . magit-reflog)
   ("C-c g t" . magit-tag))

  :config
  ;; display buffer fullframe
  (setq magit-display-buffer-function #'shadow/magit-display-buffer-function)
  ;; `git-commit-mode'
  ;; see https://chris.beams.io/posts/git-commit/
  (setq fill-column 72
        git-commit-summary-max-length 50
        git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line)))

;; Gitflow externsion for Magit
(use-package magit-gitflow
  :after magit
  :diminish magit-gitflow-mode
  :bind
  (:map magit-status-mode-map
              ("G" . magit-gitflow-popup))
  :init
  (add-hook 'magit-mode-hook #'turn-on-magit-gitflow)
  :config
  (magit-define-popup-action 'magit-dispatch-popup
    ?G "GitFlow" #'magit-gitflow-popup ?!))

;; Git modes
(use-package git-modes
  :if *git*)

(provide 'init-git)
;;; init-git.el ends here
