;;; init-helper.el --- Generic config. -*- lexical-binding: t -*-
(require 'helpful)

(setq helpful-max-buffers 10)

(defun helpful-at-point-dwim ()
  (interactive)
  (let ((symbol (symbol-at-point)))
    (if symbol (helpful-symbol symbol)
      (call-interactively #'helpful-symbol))))

(lazy-load-global-keys '(("C-h f" . helpful-callable))
                       "helpful")
(lazy-load-global-keys '(("C-h v" . helpful-variable))
                       "helpful")
(lazy-load-global-keys '(("C-h k" . helpful-key))
                       "helpful")
(lazy-load-global-keys '(("C-h x" . helpful-command))
                       "helpful")
(lazy-load-global-keys '(("C-c C-d" . helpful-at-point-dwim))
                       "helpful")

(defun helpful-reuse-window-function (buf)
  (if-let ((window (display-buffer-reuse-mode-window buf '((mode . helpful-mode)))))
      (select-window window)
    (pop-to-buffer buf)))
(setq helpful-switch-buffer-function #'helpful-reuse-window-function)

(defun helpful-previous-helpful-buffer ()
  (interactive)
  (let ((bufname (buffer-name)))
    (previous-buffer)
    (while (and (not (eq major-mode 'helpful-mode))
                (not (string= (buffer-name) bufname)))
      (previous-buffer))))

(defun helpful-next-helpful-buffer ()
  (interactive)
  (let ((bufname (buffer-name)))
    (next-buffer)
    (while (and (not (eq major-mode 'helpful-mode))
                (not (string= (buffer-name) bufname)))
      (next-buffer))))

(lazy-load-local-keys '(
                        ("," . beginning-of-buffer)
                        ("." . end-of-buffer)
                        ("b" . helpful-previous-helpful-buffer)
                        ("b" . helpful-next-helpful-buffer)
                        ("q" . delete-window)) helpful-mode-map "")
(with-eval-after-load 'evil
  (evil-define-key 'normal helpful-mode-map (kbd "q") 'quit-window))
(provide 'init-helper)
