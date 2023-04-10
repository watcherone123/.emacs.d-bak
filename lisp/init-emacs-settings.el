;;; core-emacs-settings.el --- Editor defaults

;;; Commentary:
;;

;;; Code:
(require 'init-constants)
(require 'init-funcs)

;; Font
(defvar font-list '(("Input" . 11)
                    ("Fira Code" . 11)
                    ("Source Code Pro" . 12)
                    ("Ubuntu Mono" . 13)
                    ("Consolas" . 12)
                    ("DejaVu Sans Font" . 11)
                    ("SauceCodePro Nerd Font Mono" . 11)
                    ("monofur" . 11)
                    ("Anonymous Pro" . 11))
  "List of fonts and sizes.  The first one available will be used.")

;; FontFun
(defun change-font ()
  "Documentation."
  (interactive)
  (let* (available-fonts font-name font-size font-setting)
    (dolist (font font-list (setq available-fonts (nreverse available-fonts)))
      (when (member (car font) (font-family-list))
        (push font available-fonts)))
    (if (not available-fonts)
        (message "No fonts from the chosen set are available")
      (if (called-interactively-p 'interactive)
          (let* ((chosen (assoc-string (completing-read "What font to use? " available-fonts nil t) available-fonts)))
            (setq font-name (car chosen) font-size (read-number "Font size: " (cdr chosen))))
        (setq font-name (caar available-fonts) font-size (cdar available-fonts)))
      (setq font-setting (format "%s-%d" font-name font-size))
      (set-frame-font font-setting nil t)
      (add-to-list 'default-frame-alist (cons 'font font-setting))
      (setq-local font-name-s (format "\"%s\"" font-name))
      (centaur-set-variable 'sniper-font (cons font-name-s font-size)))))

;; Fonts
(defun sniper-set-fonts ()
    "Set default font."
    (if (font-installed-p (car sniper-font))
        (set-face-attribute 'default nil
                                        :font (car sniper-font)
                                        :height (* (cdr sniper-font) 10))
      (progn (message "can't find font %s" (car sniper-font))
             (cl-loop for (font . size) in font-list
                      when (font-installed-p font)
                      return (set-face-attribute 'default nil
                                                  :font font
                                                  :height (* size 10)))))
    ;; Specify font for all unicode characters
    (set-fontset-font t 'unicode "Symbola" nil 'prepend)
    ;; Specify font for Chinese characters
    (cl-loop for font in '("WenQuanYi Micro Hei" "Microsoft Yahei")
            when (font-installed-p font)
            return (set-fontset-font t '(#x4e00 . #x9fff) font)))

(maple/add-hook 'after-init-hook
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (select-frame frame)
                  (sniper-set-fonts)))
    (when (display-graphic-p)
      (sniper-set-fonts))))

(provide 'init-emacs-settings)
;;; init-emacs-settings.el ends here
