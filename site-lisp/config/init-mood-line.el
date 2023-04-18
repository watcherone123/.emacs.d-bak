;;; init-moodline.el --- Functions for base. -*- lexical-binding: t; -*-
;;; Require


(add-hook 'after-init-hook (lambda ()
                            (require 'mood-line)
                            (mood-line-mode)
                            ))

(provide 'init-mood-line)
