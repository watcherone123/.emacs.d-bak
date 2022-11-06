;;; Require

(require 'smex)

;;; Code:

(smex-initialize)

(defun smex+ ()
  (interactive)
  (let ((resize-mini-windows nil))
    (smex)
    ))

(provide 'init-smex)