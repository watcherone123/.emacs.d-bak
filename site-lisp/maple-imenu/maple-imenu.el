;;; maple-imenu.el ---  maple imenu configuration.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2019 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; URL: https://github.com/honmaple/emacs-maple-imenu

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; maple imenu configuration.
;;

;;; Code:

(require 'imenu)
(require 'cl-lib)
(require 'subr-x)

(defconst maple-imenu-buffer-name "*maple-imenu*"
  "Name of the buffer that is used to display imenu entries.")

(defvar maple-imenu-buffer nil)
(defvar maple-imenu-overlays nil)

(defgroup maple-imenu nil
  "Display imenu in window side."
  :group 'imenu)

(defcustom maple-imenu-autoupdate t
  "Whether auto update imenu when file save or window change."
  :type 'boolean
  :group 'maple-imenu)

(defcustom maple-imenu-autoresize nil
  "Whether auto resize imenu window when item's length is long."
  :type 'boolean
  :group 'maple-imenu)

(defcustom maple-imenu-width 25
  "Display window width."
  :type 'number
  :group 'maple-imenu)

(defcustom maple-imenu-indent 2
  "Display indent."
  :type 'number
  :group 'maple-imenu)

(defcustom maple-imenu-arrow '("▾" . "▸")
  "Display arrow when show or hide entry."
  :type 'cons
  :group 'maple-imenu)

(defcustom maple-imenu-display-action '(maple-imenu--set-buffer)
  "Display buffer func."
  :type 'list
  :group 'maple-imenu)

(defcustom maple-imenu-display-alist '((side . right) (slot . -1))
  "Used by `display-buffer-in-side-window`."
  :type 'alist
  :group 'maple-imenu)

(defcustom maple-imenu-list-size 0.3
  "Size (height or width) for the imenu-list buffer.
Either a positive integer (number of rows/columns) or a percentage."
  :group 'maple-imenu
  :type 'number)

(defcustom maple-imenu-list-position 'right
  "Position of the imenu-list buffer.
Either 'right, 'left, 'above or 'below.  This value is passed
directly to `split-window'."
  :group 'maple-imenu
  :type '(choice (const above)
                 (const below)
                 (const left)
                 (const right)))

(defcustom maple-imenu-display-mode '(prog-mode org-mode)
  "The modes that be allowed to display `."
  :type 'list
  :group 'maple-imenu)

(defface maple-imenu-face
  '((t (:inherit font-lock-type-face)))
  "Default face for maple-imenu.")

(defface maple-imenu-item-face
  '((t (:inherit font-lock-variable-name-face)))
  "Default item face for maple-imenu.")

(defcustom maple-imenu-mode-line-format
  '("%e" mode-line-front-space mode-line-mule-info mode-line-client
    mode-line-modified mode-line-remote mode-line-frame-identification
    (:propertize "%b" face mode-line-buffer-id) " "
    (:eval (buffer-name maple-imenu-buffer)) " "
    mode-line-end-spaces)
  "Local mode-line format for the imenu-list buffer.
This is the local value of `mode-line-format' to use in the imenu-list
buffer.  See `mode-line-format' for allowed values."
  :group 'maple-imenu
  :type 'sexp)

(defun maple-imenu--set-mode-line ()
  "Locally change `mode-line-format' to `imenu-list-mode-line-format'."
  (setq-local mode-line-format maple-imenu-mode-line-format))
(add-hook 'maple-imenu-mode-hook #'maple-imenu--set-mode-line)

(defmacro maple-imenu--with-buffer (&rest body)
  "Execute the forms in BODY with buffer."
  (declare (indent 0) (debug t))
  `(let ((buffer (get-buffer-create maple-imenu-buffer-name)))
     (with-current-buffer buffer
       ,@body)))

(defmacro maple-imenu--with-window (&rest body)
  "Execute the forms in BODY with window."
  (declare (indent 0) (debug t))
  `(let ((window (maple-imenu-window))
         (golden-ratio-mode-p (bound-and-true-p golden-ratio-mode)))
     (when golden-ratio-mode-p (golden-ratio-mode -1))
     (when window (with-selected-window window
                    (setq window-size-fixed nil) ,@body (setq window-size-fixed 'width)))
     (when golden-ratio-mode-p (golden-ratio-mode golden-ratio-mode-p))))

(defmacro maple-imenu--with-writable (&rest body)
  "Execute the forms in BODY with window."
  (declare (indent 0) (debug t))
  `(save-excursion
     (read-only-mode -1)
     ,@body
     (read-only-mode 1)))

(defun maple-imenu--items (items)
  "Categorize all the functions of imenu with ITEMS."
  (if-let ((fns (cl-remove-if #'listp items :key #'cdr)))
      (nconc (cl-remove-if #'nlistp items :key #'cdr)
             `(("Functions" ,@fns)))
    items))

(defun maple-imenu--entries(&optional buffer)
  "Get imenu with &optional BUFFER."
  (with-current-buffer (or buffer maple-imenu-buffer (current-buffer))
    (let* ((imenu-max-item-length "Unlimited")
           (imenu-auto-rescan t)
           (items (or (ignore-errors (imenu--make-index-alist t)) (list))))
      (maple-imenu--items
       (delete (assoc "*Rescan*" items) items)))))

(defun maple-imenu--item-text(item &optional indent)
  "TEXT ITEM &OPTIONAL INDENT."
  (when (not (string= (car item) ""))
    (let* ((point (cdr item))
           (point (if (overlayp point) (overlay-start point) point))
           (text (replace-regexp-in-string "^dummy::" "" (car item)))
           (text  (format "%s%s" (make-string (or indent 0) ?\s) text)))
      (insert-button
       text
       'action
       `(lambda (_)
          (pop-to-buffer maple-imenu-buffer)
          (goto-char ,point))
       'follow-link t
       'face 'maple-imenu-item-face)
      (insert "\n"))))

(defun maple-imenu--text(text &optional indent)
  "TEXT &OPTIONAL INDENT."
  (insert-button
   (format "%s%s %s" (make-string (or indent 0) ?\s) (car maple-imenu-arrow) text)
   'action 'maple-imenu-toggle-entry
   'face 'maple-imenu-face
   'follow-link t)
  (insert "\n"))

(defun maple-imenu--handler(items &optional indent)
  "Handler ITEMS &OPTIONAL INDENT."
  (let ((keyword (car items))
        (item (cdr items))
        (indent (or indent 0)))
    (if (not (listp item))
        (maple-imenu--item-text items indent)
      (maple-imenu--text keyword indent)
      (setq indent (+ indent maple-imenu-indent))
      (dolist (arg item) (maple-imenu--handler arg indent)))))

(defun maple-imenu--set-buffer (buffer _alist)
  "Display BUFFER _ALIST."
  (display-buffer-in-side-window buffer maple-imenu-display-alist))

(defun maple-imenu--set-window ()
  "Reset window width."
  (let ((w (max maple-imenu-width window-min-width)))
    (if (> (window-width) w)
        (shrink-window-horizontally (- (window-width) w))
      (if (< (window-width) w)
          (enlarge-window-horizontally (- w (window-width)))))))

(defun maple-imenu--level()
  "Get current line level."
  (let ((text (thing-at-point 'line t)))
    (- (string-width text) (string-width (string-trim-left text)))))

(defun maple-imenu--point()
  "Get point."
  (let* ((level (maple-imenu--level))
         (point (line-end-position))
         stop)
    (save-excursion
      (while (not stop)
        (forward-line 1)
        (if (and (> (maple-imenu--level) level)
                 (< point (point-max)))
            (setq point (line-end-position))
          (setq stop t))))
    point))

(defun maple-imenu--exchange-arrow(&optional reverse)
  "Exchange arrow with &optional REVERSE."
  (maple-imenu--with-writable
    (beginning-of-line)
    (when (search-forward (if reverse (cdr maple-imenu-arrow) (car maple-imenu-arrow)) nil t)
      (replace-match (if reverse (car maple-imenu-arrow) (cdr maple-imenu-arrow))))))

(defun maple-imenu-show-entry(&optional overlay)
  "Show entry with &optional OVERLAY."
  (interactive)
  (let ((overlay (or overlay (car maple-imenu-overlays))))
    (maple-imenu--exchange-arrow t)
    (delete-overlay (cdr overlay))
    (setq maple-imenu-overlays
          (cl-remove-if
           (lambda(x) (eq (car x) (car overlay)))
           maple-imenu-overlays))))

(defun maple-imenu-hide-entry()
  "Hide entry."
  (interactive)
  (let ((new-overlay (make-overlay (line-end-position) (maple-imenu--point))))
    (maple-imenu--exchange-arrow)
    (push (cons (line-number-at-pos) new-overlay) maple-imenu-overlays)
    (overlay-put new-overlay 'invisible t)))

(defun maple-imenu-toggle-entry(&optional _)
  "Toggle entry."
  (interactive)
  (if-let ((overlay (assq (line-number-at-pos) maple-imenu-overlays)))
      (maple-imenu-show-entry overlay)
    (maple-imenu-hide-entry)))

(defun maple-imenu-update()
  "Update imenu."
  (interactive)
  (when (and (maple-imenu-window) (apply 'derived-mode-p maple-imenu-display-mode))
    (setq maple-imenu-buffer (current-buffer))
    (maple-imenu--with-buffer (maple-imenu-refresh))))

(defun maple-imenu-refresh()
  "Refresh imenu buffer."
  (interactive)
  (maple-imenu--with-writable
    (erase-buffer)
    (dolist (item (maple-imenu--entries))
      (maple-imenu--handler item)))
  (when maple-imenu-autoresize
      (maple-imenu--with-window
        (let ((fit-window-to-buffer-horizontally t))
          (fit-window-to-buffer)
          (fit-window-to-buffer nil nil nil nil (+ (window-width) 6))
          ))))

(defun imenu-list-split-size ()
  "Convert `imenu-list-size' to proper argument for `split-window'."
  (let ((frame-size (if (member maple-imenu-list-position '(left right))
                        (frame-width)
                      (frame-height))))
    (cond ((integerp maple-imenu-list-size) (- maple-imenu-list-size))
          (t (- (round (* frame-size maple-imenu-list-size)))))))

(defun imenu-list-display-buffer (buffer alist)
  "Display the imenu-list buffer at the side.
This function should be used with `display-buffer-alist'.
See `display-buffer-alist' for a description of BUFFER and ALIST."
  (or (get-buffer-window buffer)
      (let ((window (ignore-errors (split-window (frame-root-window) (imenu-list-split-size) maple-imenu-list-position))))
        (when window
          ;; since Emacs 27.0.50, `window--display-buffer' doesn't take a
          ;; `dedicated' argument, so instead call `set-window-dedicated-p'
          ;; directly (works both on new and old Emacs versions)
          (window--display-buffer buffer window 'window alist)
          (set-window-dedicated-p window t)
          window))))

(defun imenu-list-install-display-buffer ()
  "Install imenu-list display settings to `display-buffer-alist'."
  (cl-pushnew `(,(concat "^" (regexp-quote maple-imenu-buffer-name) "$")
                imenu-list-display-buffer)
              display-buffer-alist
              :test #'equal))

(defun imenu-list-purpose-display-condition (_purpose buffer _alist)
  "Display condition for use with window-purpose.
Return t if BUFFER is the imenu-list buffer.

This function should be used in `purpose-special-action-sequences'.
See `purpose-special-action-sequences' for a description of _PURPOSE,
BUFFER and _ALIST."
  (string-equal (buffer-name buffer) maple-imenu-buffer-name))

;; hide false-positive byte-compile warning
(defvar purpose-special-action-sequences)

(defun imenu-list-install-purpose-display ()
  "Install imenu-list display settings for window-purpose.
Install entry for imenu-list in `purpose-special-action-sequences'."
  (cl-pushnew '(imenu-list-purpose-display-condition imenu-list-display-buffer)
              purpose-special-action-sequences
              :test #'equal))

(imenu-list-install-display-buffer)
(eval-after-load 'window-purpose
  '(imenu-list-install-purpose-display))

(defun maple-imenu-show ()
  "Show."
  (interactive)
  (setq maple-imenu-buffer (current-buffer))
  (maple-imenu--with-buffer
    (maple-imenu-mode)
    (maple-imenu-refresh)
    (when (= (buffer-size) 0)
      (maple-imenu-hide)
      (message "no imenu found"))))

(defun maple-imenu-hide ()
  "Hide."
  (interactive)
  (when-let ((window (maple-imenu-window)))
    (delete-window window))
  (setq maple-imenu-buffer nil)
  (remove-hook 'after-save-hook 'maple-imenu-update)
  (remove-hook 'window-configuration-change-hook 'maple-imenu-update))

(defun maple-imenu-window ()
  "Whether show maple-imenu."
  (get-buffer-window maple-imenu-buffer-name t))

(defun maple-imenu ()
  "Toggle open and close."
  (interactive)
  (if (maple-imenu-window) (maple-imenu-hide) (maple-imenu-show)))

(defvar maple-imenu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] #'maple-imenu-toggle-entry)
    (define-key map (kbd "r") #'maple-imenu-refresh)
    (define-key map (kbd "q") #'maple-imenu-hide)
    (define-key map (kbd "h") #'backward-char)
    (define-key map (kbd "l") #'forward-char)
    map)
  "Maple-imenu-mode keymap.")

;;;###autoload
(define-derived-mode maple-imenu-mode special-mode "maple-imenu"
  "Maple-imenu-mode."
  (setq indent-tabs-mode nil
        buffer-read-only t
        truncate-lines -1
        cursor-in-non-selected-windows nil)
  (select-window (display-buffer maple-imenu-buffer-name maple-imenu-display-action))
  (maple-imenu--with-window (maple-imenu--set-window))
  (when maple-imenu-autoupdate
    (add-hook 'after-save-hook 'maple-imenu-update)
    (add-hook 'window-configuration-change-hook 'maple-imenu-update)))

(provide 'maple-imenu)
;;; maple-imenu.el ends here
