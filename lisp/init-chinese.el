;;; init-packages.el --- chinese package config for emacs.-*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package go-translate
  :ensure t
  :defer t
  :commands (my-default-translator
              bing-translate-pop)
  :config
  (setq gts-translate-list '(("en" "zh")))
  (setq gts-default-translator
        (gts-translator
        :picker (gts-noprompt-picker)
        :engines (gts-google-engine)
        :render
        (gts-posframe-pop-render)))

  (defvar bing-translator-pop
    (gts-translator :picker (gts-noprompt-picker)
    			    :engines (gts-bing-engine)
    			    :render (gts-posframe-pop-render)))

  (defun bing-translate-pop ()
    (interactive)
    (gts-translate bing-translator-pop))

  (defvar my-default-translator
    (gts-translator :picker (gts-prompt-picker)
                    :engines (list (gts-google-engine) (gts-bing-engine) (gts-google-rpc-engine))
                    ;; (gts-posframe-pin-render :position (cons 1200 20))
    			    :render (gts-buffer-render)))
  (defun my-default-translate ()
    (interactive)
    (gts-translate my-default-translator)))

(provide 'init-chinese)
;;; init-chinese.el ends here
