;; init-dashboard.el --- Initialize dashboard configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:

(setq user-full-name "sniper")

(use-package sniper-dashboard
  :ensure nil; local package
  :load-path "site-lisp/dashboard"
  :bind ("<f2>" . sniper/open-dashboard)
  :hook (window-setup . blove/dashboard-setup)
  :init
  (general-define-key
   :states 'normal
   :keymaps 'sniper-dashboard-mode-map
   "k" 'sniper-dashboard-previous-section
   "j" 'sniper-dashboard-next-section
   "RET" 'sniper-dashboard-return)
  :config
  (setq sniper-dashboard-logo-title "Emacs ♥ You - Enjoy Programming & Writing"
        sniper-dashboard-footer-messages (list (format "Powered by %s, %s" user-full-name (format-time-string "%Y")))
        sniper-dashboard-footer-icon (if (display-graphic-p)
                                  (all-the-icons-faicon "heart"
                                                        :height 1.1
                                                        :v-adjust -0.05
                                                        :face 'error)
                                "♥")))

(provide 'init-dashboard)

;;; init-dashboard.el ends here
