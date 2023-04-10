;;; custom.el --- user customization file    -*- no-byte-compile: t -*-
;;; Commentary:
;;;       Add or change the configurations in custom.el, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:

;; (setq centaur-logo nil)                        ; Logo file or nil (official logo)
;; (setq centaur-full-name "user name")           ; User full name
;; (setq centaur-mail-address "user@email.com")   ; Email address
;; (setq centaur-package-archives 'emacs-china)   ; Package repo: melpa, melpa-mirror, emacs-china, netease, ustsc, tencent or tuna
;; (setq sniper-theme 'doom-one)                  ; Color theme: doom-solarized-dark, doom-monokai-pro, modus-operandi, doom-one
;; (setq sniper-font '("Source Code Pro" . 12))   ; font: "Fira Code", "Source Code Pro", "Ubuntu Mono", "Consolas"
;; (setq centaur-dashboard nil)                   ; Use dashboard at startup or not: t or nil
;; (setq use-rime nil)                            ; Use rime or not: t or nil
;; (setq centaur-prettify-symbols-alist nil)      ; Alist of symbol prettifications
;; (setq centaur-benchmark-init t)                ; Enable initialization benchmark or not: t or nil

;; Misc.
;; (setq confirm-kill-emacs 'y-or-n-p)

;; Display on the specified monitor
;; (when (and (> (length (display-monitor-attributes-list)) 1)
;;            (> (display-pixel-width) 1920))
;;   (set-frame-parameter nil 'left 1920))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; custom.el ends here
