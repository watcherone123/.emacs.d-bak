;;; init-edit.el  --- edit. -*- lexical-binding: t -*-

;; insert pairs
(defun insert-quotations (&optional arg)
  "Enclose following ARG sexps in quotation marks.
    Leave point after open-paren."
  (interactive "*P")
  (insert-pair arg ?\' ?\'))

(defun insert-quotes (&optional arg)
  "Enclose following ARG sexps in quotes.
    Leave point after open-quote."
  (interactive "*P")
  (insert-pair arg ?\" ?\"))

(defun insert-backquote (&optional arg)
  "Enclose following ARG sexps in quotations with backquote.
    Leave point after open-quotation."
  (interactive "*P")
  (insert-pair arg ?\` ?\'))

(defun insert-star (&optional arg)
  "Enclose following ARG sexps in stars.
  Leave point after open-quotation."
  (interactive "*P")
  (insert-pair arg ?\* ?\*))

(defun insert-bracket (&optional arg)
  "Enclose following ARG sexps in brackets.
  Leave point after open-quotation."
  (interactive "*P")
  (insert-pair arg ?\[ ?\]))

(defun insert-curly (&optional arg)
  "Enclose following ARG sexps in curly braces.
  Leave point after open-quotation."
  (interactive "*P")
  (insert-pair arg ?\{ ?\}))

(defun insert-equate (&optional arg)
  "Enclose following ARG sexps in equations.
  Leave point after open-quotation."
  (interactive "*P")
  (insert-pair arg ?\= ?\=))

(provide 'init-iedit)

;;; init-iedit.el ends here
