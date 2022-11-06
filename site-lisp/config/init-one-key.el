;;; Require
(require 'eaf)
(require 'eaf-file-manager)

;;; Code:

(one-key-create-menu
 "DIRECTORY"
 '(
   (("h" . "Home") . (lambda () (interactive) (eaf-open-in-file-manager "~/")))
   (("b" . "Book") . (lambda () (interactive) (eaf-open-in-file-manager "/data/Book")))
   (("j" . "Picture") . (lambda () (interactive) (eaf-open-in-file-manager "/data/Picture")))
   (("m" . "Music") . (lambda () (interactive) (eaf-open-in-file-manager "/data/Music")))
   (("c" . "Config") . (lambda () (interactive) (eaf-open-in-file-manager lazycat-emacs-config-dir)))
   ((";" . "Extension") . (lambda () (interactive) (eaf-open-in-file-manager "~/.emacs.d")))
   )
 t)

(provide 'init-one-key)