;;; sniper-dashboard.el --- A minimal dashboard inspired by emacs-dashboard -*- lexical-binding: t; -*-

;; Author: sniper
;; Homepage: https://gitee.com/e190/emacs.d
;; Keywords: dashboard
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; sniper-modeline is a minimal mode-line configuration that aims to replicate
;; some of the features of the doom-modeline package.
;;
;; Features offered:
;; * Clean, minimal design
;; * Anzu and multiple-cursors counter
;; * Version control status indicator
;; * Flycheck status indicator
;; * Flymake support
;; * Lightweight with no dependencies

;;; Code:

(declare-function all-the-icons-fileicon "ext:data-fileicons.el")
;;; === Defconst
(defconst dashboard-startup-banner (expand-file-name "img/dashLogo.png" user-emacs-directory)
  "Blove-ashboard's logo.")

(defconst dashboard-startup-banner-default (expand-file-name "img/banner.txt" user-emacs-directory)
  "Blove-ashboard's default logo.")

(defcustom sniper-dashboard-logo-title "Welcome to Emacs!"
  "Specify the startup banner."
  :type 'string
  :group 'dashboard)

(defcustom sniper-dashboard-footer-messages
  '("The one true editor, Emacs!"
    "Who the hell uses VIM anyway? Go Evil!"
    "Free as free speech, free as free Beer"
    "Happy coding!"
    "Vi Vi Vi, the editor of the beast"
    "Welcome to the church of Emacs"
    "While any text editor can save your files, only Emacs can save your soul"
    "I showed you my source code, pls respond")
  "A list of messages, one of which dashboard chooses to display."
  :type 'list
  :group 'dashboard)

(defcustom sniper-dashboard-footer
  (nth (random (1- (1+ (length sniper-dashboard-footer-messages)))) sniper-dashboard-footer-messages)
  "A footer with some short message."
  :type 'string
  :group 'dashboard)

(defcustom dashboard-items-default-length 20
  "Length used for startup lists with otherwise unspecified bounds.
Set to nil for unbounded."
  :type  'integer
  :group 'dashboard)

(defcustom sniper-dashboard-set-footer t
  "When non nil, a footer will be displayed at the bottom."
  :type 'boolean
  :group 'dashboard)

(defcustom sniper-dashboard-footer-icon
  (if (and (display-graphic-p)
           (or (fboundp 'all-the-icons-fileicon)
               (require 'all-the-icons nil 'noerror)))
      (all-the-icons-fileicon "emacs"
                              :height 1.1
                              :v-adjust -0.05
                              :face 'font-lock-keyword-face)
    (propertize ">" 'face 'dashboard-logo-title-face))
  "Footer's icon."
  :type 'string
  :group 'dashboard)

(defconst blove/dashboard-buf-name "*BloveDashboard*"
  "Dashboard's buffer name.")

(defconst show-recent-file-nums 12
  "Recentf nums.")

(defconst blove/full-path t
  "Full path name or base-name. :t | nil .")

(defvar sniper-dashboard-path-max-length 20
  "Maximum length for path to display.")

(defvar recent-file-alist nil)
(defvar recent-file-num 0)

(defface dashboard-logo-title-face
  '((t :inherit default))
  "Face used for the banner title."
  :group 'dashboard)

(defface dashboard-pack-time-face
  '((t :inherit (shadow)))
  "Face used for the banner title."
  :group 'dashboard)

(defface dashboard-text-banner
  '((t (:inherit font-lock-keyword-face)))
  "Face used for text banners."
  :group 'dashboard)

(defface widget-button '((t (:weight bold)))
  "Face used for widget buttons."
  :group 'widget-faces)

(defface dashboard-no-items-face
  '((t (:inherit widget-button)))
  "Face used for no items."
  :group 'dashboard)


(defconst sniper-dashboard-banner-length 75
  "Width of a banner.")

;; Custom splash screen
(defvar sniper-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<up>") 'sniper-dashboard-previous-section)
    (define-key map (kbd "<down>") 'sniper-dashboard-next-section)
    (define-key map (kbd "k") 'sniper-dashboard-previous-section)
    (define-key map (kbd "j") 'sniper-dashboard-next-section)
    (define-key map (kbd "q") 'shadow/quit-dashboard)
    (define-key map [tab] 'sniper-dashboard-next-section)
    (define-key map (kbd "RET") 'sniper-dashboard-return)
    map)
  "Keymap for dashboard mode.")

(define-derived-mode sniper-dashboard-mode special-mode "Dashboard"
  "Dashboard major mode for startup screen."
  (buffer-disable-undo)
  (when (featurep 'whitespace) (whitespace-mode -1))
  (when (featurep 'linum) (linum-mode -1))
  (when (featurep 'display-line-numbers) (display-line-numbers-mode -1))
  (setq inhibit-startup-screen t
        buffer-read-only t
        truncate-lines t))

;; Footer
(defun dashboard--random-footer ()
  "Return a random footer from `dashboard-footer-messages'."
  (nth (random (length sniper-dashboard-footer-messages)) sniper-dashboard-footer-messages))

(defun dashboard--insert-footer ()
  "Insert footer of dashboard."
  (when-let ((footer (and sniper-dashboard-set-footer (dashboard--random-footer))))
    (insert "\n")
    (dashboard--center-line footer)
    (insert sniper-dashboard-footer-icon)
    (insert " ")
    (insert (propertize footer 'face 'font-lock-doc-face))
    (insert "\n")))

(defun dashboard--center-line (string)
  "Center a STRING accoring to it's size."
  (insert (make-string (max 0 (floor (/ (- sniper-dashboard-banner-length
                                           (+ (length string) 1)) 2))) ?\ )))
;;; ~ insert all-the-icons-icon-for-file
(defun blove/insert--icons (item)
  "Return the all-the-icons-icon-for-file by ITEM."
  (insert (all-the-icons-icon-for-file (concat "ext:" item)
									   :height 1.0
									   :v-adjust -0.05
									   )))

;;; ~ make file link
(defun blove/make-dashboard-link (label link)
  "Return the file with full path [LABEL LINK]."
  (insert-button label
				 'action (lambda (_) (find-file link))
				 'follow-link t
				 'face 'bold)
  (insert "\n"))

;;; ~ show packages % init-time
(defun blove/pack-time ()
  "Return time."
  (let ((package-count 0) (time (emacs-init-time)))
    (when (bound-and-true-p package-alist)
      (setq package-count (length package-activated-list)))
    (if (zerop package-count)
        (format "Emacs started in %s" time)
      (format "%d packages loaded in %s" package-count time))))

(defun dashboard--save-recentf-alist (file)
  "FILE."
  (let* ((line-file (cons (line-number-at-pos) file)))
    (setq recent-file-alist (append recent-file-alist (list line-file)))))

(defun dashboard--goto-line (line)
  "Goto LINE."
  (goto-char (point-min)) (forward-line (1- line)))

(defun dashboard--get-line (n next)
  "N `line-number-at-pos', NEXT is bool."
  (if recent-file-alist
    (let ((recentf-first-item (car (nth 0 recent-file-alist)))
          (recentf-end-item (+ (car (nth 0 recent-file-alist)) (1- recent-file-num))))
      (if next
          (cond ((< n recentf-first-item) (car (nth 0 recent-file-alist)))
                ((>= n recentf-end-item) recentf-first-item)
                (t (+ n 1)))
          (cond ((<= n recentf-first-item) recentf-end-item)
                ((> n recentf-end-item) recentf-end-item)
                (t (- n 1)))))
    (if next
        (+ (line-number-at-pos) 1)
        (- (line-number-at-pos) 1))
    ))

(defun sniper-dashboard-next-section ()
  "Navigate forward to next section."
  (interactive)
  (dashboard--goto-line (dashboard--get-line (line-number-at-pos) t)))

(defun sniper-dashboard-previous-section ()
  "Navigate back to previous section."
  (interactive)
  (dashboard--goto-line (dashboard--get-line (line-number-at-pos) nil)))

(defun sniper-dashboard-return ()
  "Navigate forward to next section."
  (interactive)
  (let* ((file-name (assoc (line-number-at-pos) recent-file-alist)))
    (if file-name
        (find-file (cdr file-name))
      (sniper-dashboard-next-section))))

(defun sniper-dashboard-choose-banner()
  (if (or (string-suffix-p ".txt" dashboard-startup-banner)
        (and (display-graphic-p)
          (image-type-available-p (intern (file-name-extension
                                          dashboard-startup-banner)))))
    dashboard-startup-banner dashboard-startup-banner-default))

(defun dashboard-insert-ascii-banner-centered (file)
  "Insert banner from FILE."
  (let ((ascii-banner
         (with-temp-buffer
           (insert-file-contents file)
           (let ((banner-width 0))
             (while (not (eobp))
               (let ((line-length (- (line-end-position) (line-beginning-position))))
                 (if (< banner-width line-length)
                     (setq banner-width line-length)))
               (forward-line 1))
             (goto-char 0)
             (let ((margin
                    (max 0 (floor (/ (- sniper-dashboard-banner-length banner-width) 2)))))
               (while (not (eobp))
                 (insert (make-string margin ?\ ))
                 (forward-line 1))))
           (buffer-string))))
    (put-text-property 0 (length ascii-banner) 'face 'dashboard-text-banner ascii-banner)
    (insert ascii-banner)))

(defun dashboard-insert-image-banner (banner)
  "Display an image BANNER."
  (let ((img-fd (create-image banner)))
    (insert (propertize " " 'display `(space :align-to (+ center (-0.5 . ,img-fd)))))
    (when (display-graphic-p)
      (insert-image img-fd))))

(defun dashboard-insert-banner ()
  "Insert Banner at the top of the dashboard."
  (goto-char (point-max))
  (let ((banner (sniper-dashboard-choose-banner)) buffer-read-only)
    (when banner
      (if (image-type-available-p (intern (file-name-extension banner)))
          (dashboard-insert-image-banner banner)
        (dashboard-insert-ascii-banner-centered banner)))))

(defun sniper-dashboard--insert-recentf()
  "Insert recentf list."
  (require 'recentf)
  (recentf-mode)

  (unless recentf-list
    (message "No recentf items")
    (sniper-dashboard--insert-text "--- No items ---" 'dashboard-no-items-face))

  (when recentf-list
    (if (< (length recentf-list) show-recent-file-nums)
        (setq recent-file-num (length recentf-list))
      (setq recent-file-num show-recent-file-nums))

    (dotimes (count recent-file-num)
      (setq label (if blove/full-path (abbreviate-file-name (nth count recentf-list))
                    (file-name-nondirectory (nth count recentf-list))))
      (setq sniper-dashboard-path-max-length
            (if (< sniper-dashboard-path-max-length (length label))
                (length label) sniper-dashboard-path-max-length)))
    (setq recent-file-alist nil)
    (mapcar ;; === loop start
      (lambda (item) ;; --- mani function start
        ;;; ~~~ insert all-the-icons
        (setq label (concat " "
                      (if blove/full-path (abbreviate-file-name item) (file-name-nondirectory item))))
        (insert (make-string (max 0 (floor (/ (- sniper-dashboard-banner-length
                                                (+ sniper-dashboard-path-max-length 1)) 2))) ?\ ))
        (blove/insert--icons item)
        ;;; ~~~ insert all-the-icons end
        (dashboard--save-recentf-alist item)
        (blove/make-dashboard-link label item)
        ) ;;; --- main function end
      ;;; loop in iterms
      (cl-loop for i from 0 to (- recent-file-num 1)
              collect (nth i recentf-list) into rflst
              finally return rflst))))

(defun sniper-dashboard--insert-text(text face)
  "Insert text [TEXT FACE] ."
    (dashboard--center-line text)
    (insert (format "%s\n\n" (propertize text 'face face))))

(defcustom dashboard-buffer-last-width nil
  "Previous width of dashboard-buffer."
  :type  'integer
  :group 'dashboard)

(defun sniper-dashboard-insert-startupify-lists ()
  "Insert the list of widgets into the buffer."
  (interactive)
  (let ((buffer-exists (buffer-live-p (get-buffer blove/dashboard-buf-name))))
    (when (or (not (eq dashboard-buffer-last-width (window-width)))
              (not buffer-exists))
      (setq sniper-dashboard-banner-length (window-width)
            dashboard-buffer-last-width sniper-dashboard-banner-length)
      (with-current-buffer (get-buffer-create blove/dashboard-buf-name)
        (let* ((inhibit-read-only t))
          (erase-buffer)
          (dashboard-insert-banner)
          (insert "\n\n\n\n")
          (sniper-dashboard--insert-text sniper-dashboard-logo-title 'dashboard-logo-title-face)
          ;;; === insert packages and emacs-init-time ===
          (sniper-dashboard--insert-text (blove/pack-time) 'dashboard-pack-time-face)
          (sniper-dashboard--insert-recentf)
          (insert "\n\n")
          (dashboard--insert-footer))
        (when recent-file-alist
          (sniper-dashboard-next-section))
      (sniper-dashboard-mode)))))

;;; ~ main function
;;;###autoload
(defun blove/dashboard-setup ()
  "Main dashboard init."
  (sniper-dashboard-insert-startupify-lists)
  (switch-to-buffer blove/dashboard-buf-name))

(defun dashboard-resize-on-hook (&optional _)
  "Re-render dashboard on window size change."
  (let ((space-win (get-buffer-window blove/dashboard-buf-name))
        (frame-win (frame-selected-window)))
    (when (and space-win
               (not (window-minibuffer-p frame-win)))
      (with-selected-window space-win
        (sniper-dashboard-insert-startupify-lists)))))

(add-hook 'window-setup-hook
          (lambda ()
            (add-hook 'window-size-change-functions 'dashboard-resize-on-hook)
            (dashboard-resize-on-hook)))

(defun sniper/open-dashboard ()
  "Open the *dashboard* buffer and jump to the first widget."
  (interactive)
  (delete-other-windows)
  ;; Refresh dashboard buffer
  (if (get-buffer blove/dashboard-buf-name)
      (kill-buffer blove/dashboard-buf-name))
  (sniper-dashboard-insert-startupify-lists)
  (switch-to-buffer blove/dashboard-buf-name))

(defun shadow/quit-dashboard ()
  "Quit dashboard window."
  (interactive)
  (quit-window t))

;;
;; Provide sniper-modeline
;;
(provide 'sniper-dashboard)

;;; sniper-dashboard.el ends here
