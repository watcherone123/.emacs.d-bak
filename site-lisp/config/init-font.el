;;;###autoload
(defun +sky/set-cn-fonts ()
  (interactive)
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font
     "fontset-default"
     charset
     (font-spec :name +sky/cn-font
                :weight 'bold
                :slant 'normal
                :size +sky/font-size))))
;;;###autoload
(defun +sky/set-fonts ()
  (interactive)
  (when (window-system)
    (progn
      ;; 设置Emoji字体
      (let ((fonts '("Noto Color Emoji")))
        (cl-loop with script = (if (>= emacs-major-version 28)
        'emoji 'unicode)
                 for font in fonts
                 when (member font (font-family-list))
                 return (set-fontset-font t script (font-spec
						    :family font) nil 'prepend)))
      ;; 设置default face字体
      (set-face-attribute
       'default nil
       :font (font-spec :family +sky/en-font
                        :weight 'normal
                        :slant 'normal
                        :size +sky/font-size))
      ;; 设置fixed-pitch-serif face字体
      (set-face-attribute
       'fixed-pitch-serif nil
       :font (font-spec :family +sky/en-font
                        :weight 'normal
                        :slant 'italic
                        :size +sky/font-size))
      (+evan/set-cn-fonts))))

(setq +sky/en-font "Sarasa Mono SC"
      +sky/cn-font "LXGW WenKai Mono"
      +sky/font-size 15.5)



(provide 'init-font)
