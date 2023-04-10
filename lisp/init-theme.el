;; init-theme.el --- Initialize theme configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'init-funcs)

(defcustom sniper-theme 'ef-bio
  "Completion display style."
  :group 'centaur
  :type '(choice (const :tag "solarized-dark" doom-solarized-dark)
                 (const :tag "doom-one" doom-one)
                 (const :tag "light" light)
                 (const :tag "ef-bio" ef-bio)
                 (const :tag "monokai" doom-monokai-pro)))

;;(message "sniper-theme %s" sniper-theme)

(defun sniper--load-theme (theme)
  "Disable others and enable new one."
  (when theme
    (disable-theme sniper-theme)
    (load-theme theme t)
    (message "Loaded theme `%s'" theme)))

(defun sniper-load-theme (theme &optional no-save)
  "Load color THEME. Save to `custom-file' if NO-SAVE is nil."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar #'symbol-name
				     (custom-available-themes))))))
  (unless (custom-theme-name-valid-p theme)
    (error "Invalid theme name `%s'" theme))
  ;; Set option
  (sniper--load-theme theme)
  (centaur-set-variable 'sniper-theme theme no-save))

(use-package doom-themes)

;; Colorize color names in buffers
(use-package rainbow-mode
  :disabled
  :defer t
  :ensure t
  :diminish rainbow-mode
  :hook ((text-mode . rainbow-mode)
         (prog-mode . rainbow-mode)))

(use-package ef-themes
  :ensure t
  :bind ("C-c t" . ef-themes-toggle)
  :init
  ;; set two specific themes and switch between them
  (setq ef-themes-to-toggle '(ef-spring ef-bio))
  ;; set org headings and function syntax
  (setq ef-themes-headings
        '((0 . (bold 1))
          (1 . (bold 1))
          (2 . (rainbow bold 1))
          (3 . (rainbow bold 1))
          (4 . (rainbow bold 1))
          (t . (rainbow bold 1))))
  (setq ef-themes-region '(intense no-extend neutral))
  ;; Disable all other themes to avoid awkward blending:
  (mapc #'disable-theme custom-enabled-themes)

  ;; Load the theme of choice:
  ;; The themes we provide are recorded in the `ef-themes-dark-themes',
  ;; `ef-themes-light-themes'.

  ;; 如果你不喜欢随机主题，也可以直接固定选择一个主题，如下：
  ;; (ef-themes-select 'ef-summer)

  ;; 随机挑选一款主题，如果是命令行打开Emacs，则随机挑选一款黑色主题
  ;; (if (display-graphic-p)
  ;;     (ef-themes-load-random)
  ;;   (ef-themes-load-random 'dark))

  :config
  ;; auto change theme, aligning with system themes.
  (defun my/apply-theme (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (if (display-graphic-p) (ef-themes-load-random 'light) (ef-themes-load-random 'dark)))
      ('dark (ef-themes-load-random 'dark))))
  )

(maple/add-hook 'after-init-hook
  (load-theme sniper-theme t))

(provide 'init-theme)
;;; init-theme.el ends here
