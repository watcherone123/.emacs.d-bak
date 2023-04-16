;; init-buffer.el -- initialize buffer configurations. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(defun shadow/copy-clipboard-to-whole-buffer ()
  "Copy clipboard and replace buffer."
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

(defun sky-indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun sky-indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (sky-indent-buffer)
        (message "Indented buffer.")))))

(defun shadow/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard."
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

;; our own implementation of kill-this-buffer from menu-bar.el
(defun shadow/kill-this-buffer (&optional arg)
  "Kill the current buffer.
If the universal prefix ARG is non-nil then also kill the window."
  (interactive "P")
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (if (equal '(4) arg)
        (kill-buffer-and-window)
      (kill-buffer))))

;; Found at http://emacswiki.org/emacs/KillingBuffers.
(defun shadow/kill-other-buffers (&optional arg)
  "Kill all other buffers.
If the universal prefix ARG is non-nil then kill the windows too."
  (interactive "P")
  (when (yes-or-no-p (format "Killing all buffers except \"%s\"? "
                             (buffer-name)))
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (when (equal '(4) arg) (delete-other-windows))
    (message "Buffers deleted!")))

(defun shadow/new-empty-buffer ()
  "Create a new buffer called untitled(<n>)."
  (interactive)
  (let ((newbuf (generate-new-buffer-name "untitled")))
    (switch-to-buffer newbuf)
    (prog-mode)))

(defun shadow/switch-to-scratch-buffer ()
  "Switch to the `*scratch*' buffer.
Create the *scratch* buffer first if needed."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))

;;;###autoload
(defun shadow/create-scratch-buffer nil
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (erase-buffer)
  (lisp-interaction-mode))

(use-package ibuffer
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-display-summary nil)
  (setq ibuffer-use-other-window nil)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-movement-cycle nil)
  (setq ibuffer-default-sorting-mode 'filename/process)
  (setq ibuffer-use-header-line t)
  (setq ibuffer-default-shrink-to-minimum-size nil)
  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 30 30 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename)))
  (setq ibuffer-saved-filter-groups nil)

  ;;ibuffer分组
  (setq ibuffer-saved-filter-groups
        '(("Default"
          ("Hidden(g则不显示此分组)"  (name . "^ "))
          ("Helm"  (or (name . "^\\*helm\\|^\\*ac-mode-")))
          ("Woman"  (name . "^\\*WoMan.*\\*$"))
          ("Compile"  (name . "^*.*compil[ea].*$"))
          ("ERC"  (mode . erc-mode))
          ("Custom"  (mode . Custom-mode))
          ("VC"  (or (name . "magit-") (name . "^\\*vc")(mode . diff-mode) (mode . vc-dir-mode)))
          ("Magit "  (name . "magit:"))
          ("Emacs"  (name . "^\\*.*$"))
          ("Dired"  (mode . dired-mode))
          ("Shell"  (or (mode . shell-mode) (mode . vterm-mode)))
          )))

  (add-hook 'ibuffer-mode-hook
            (lambda ()
              ;; (ibuffer-auto-mode t)       ;自动更新*Ibuffer* buffer
              (ibuffer-switch-to-saved-filter-groups "Default")))

  :hook (ibuffer-mode-hook . hl-line-mode)
  :bind ("C-x C-b" . ibuffer))

(provide 'init-buffer)

;;; init-buffer.el ends here
