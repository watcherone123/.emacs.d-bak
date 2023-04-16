;;; init.el --- Emacs Initialization File
;;
;;; Commentary:
;;
;; Author: Shadow <e190@163.com>
;; URL: https://github.com/e190
;;
;; This file is not part of GNU Emacs.

;;; Code:

(let ((minver "28.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires %s or higher" minver)))

;; (setq debug-on-error t)

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 0.8MB.  Measured in bytes.
;; (setq gc-cons-threshold (* 50 1000 1000))
;; Portion of heap used for allocation.  Defaults to 0.1.
;; (setq gc-cons-percentage 0.6)

;; Speed up startup
(defvar sniper-gc-cons-threshold (if (display-graphic-p) 16000000 1600000)
  "The default value to use for `gc-cons-threshold'. If you experience freezing,
decrease this. If you experience stuttering, increase this.")

(defvar sniper-gc-cons-upper-limit (if (display-graphic-p) 400000000 100000000)
  "The temporary value for `gc-cons-threshold' to defer it.")

(defvar sniper-gc-timer (run-with-idle-timer 10 t #'garbage-collect)
  "Run garbarge collection when idle 10s.")

(defvar default-file-name-handler-alist file-name-handler-alist)

;; (setq file-name-handler-alist nil)
(setq gc-cons-threshold sniper-gc-cons-upper-limit
      gc-cons-percentage 0.5)

;; Reset file-name-handler-alist after initialization
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after startup."
            (setq file-name-handler-alist default-file-name-handler-alist
                  gc-cons-threshold sniper-gc-cons-threshold
                  gc-cons-percentage 0.1)

            ;; GC automatically while unfocusing the frame
            ;; `focus-out-hook' is obsolete since 27.1
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'focus-out-hook 'garbage-collect))

            ;; Avoid GCs while using `ivy'/`counsel'/`swiper' and `helm', etc.
            ;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
            (defun my-minibuffer-setup-hook ()
              (setq gc-cons-threshold sniper-gc-cons-upper-limit))

            (defun my-minibuffer-exit-hook ()
              (setq gc-cons-threshold sniper-gc-cons-threshold))

            (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)))

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
;; (defun update-load-path (&rest _)
;;   "Update `load-path'."
;;   (dolist (dir '("site-lisp" "lisp"))
;;     (push (expand-file-name dir user-emacs-directory) load-path)))

;; (defun add-subdirs-to-load-path (&rest _)
;;   "Add subdirectories to `load-path'."
;;   (let ((default-directory
;;           (expand-file-name "site-lisp" user-emacs-directory)))
;;     (normal-top-level-add-subdirs-to-load-path)))

;; (advice-add #'package-initialize :after #'update-load-path)
;; (advice-add #'package-initialize :after #'add-subdirs-to-load-path)

;; (update-load-path)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lib/corfu/extensions" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lib/vertico/extensions" user-emacs-directory))

(setq custom-file (concat user-emacs-directory "custom.el"))
(let ((custom-example-file
       (expand-file-name "custom-example.el" user-emacs-directory)))
  (if (and (file-exists-p custom-example-file)
           (not (file-exists-p custom-file)))
      (copy-file custom-example-file custom-file)))

;; Packages
;; Must come first
;; Packages
;; Must come first
;; (require 'init-constants)
;; @see https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;; Normally file-name-handler-alist is set to
;; (("\\`/[^/]*\\'" . tramp-completion-file-name-handler)
;; ("\\`/[^/|:][^/|]*:" . tramp-file-name-handler)
;; ("\\`/:" . file-name-non-special))
;; Which means on every .el and .elc file loaded during start up, it has to runs those regexps against the filename.
(let* ((file-name-handler-alist nil))
  (require 'init-constants)
  (require 'init-packages)
  (require 'init-funcs)
  (require 'init-base)
  (require 'init-emacs-settings)
  (require 'init-keybindings)
  (require 'init-evil)
  (require 'init-ui)
  (require 'init-edit)

  ;; Modules
  (require 'init-completing)
  (require 'init-buffer)
  (require 'init-markdown)
  (require 'init-dired)
  (require 'init-helper)
  (require 'init-search)
  (require 'init-org)
  (require 'init-lsp)
  (require 'init-prog)
  (require 'init-theme)
  (require 'init-highlight)
  ;; (require 'init-yasnippet)
  (require 'init-git)
  (require 'init-dashboard)
  (require 'init-chinese)
  )

(when (file-exists-p custom-file)
  (load custom-file))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
