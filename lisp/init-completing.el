;;; init-completing --- completing-read -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; My Emacs completing framework setup
;; - selectrum + prescient, the incremental search package, setup
;; - vertico + orderless
;;

;;; Code:
(require 'init-constants)

(use-package consult
  :defer t
  :bind (([remap imenu]                         . consult-imenu)
         ([remap bookmark-jump]                 . consult-bookmark)
         ([remap recentf-open-files]            . consult-recent-file)
         ([remap evil-show-marks]               . consult-mark)
         ([remap goto-line]                     . consult-goto-line)
         ([remap switch-to-buffer]              . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
         ([remap yank-pop]                      . consult-yank-pop))

  :commands (consult-buffer)
  :init
  ;; Custom command wrappers. It is generally encouraged to write your own
  ;; commands based on the Consult commands. Some commands have arguments which
  ;; allow tweaking. Furthermore global configuration variables can be set
  ;; locally in a let-binding.
  (defun my-fdfind (&optional dir)
    (interactive "P")
    (let ((consult-find-command '("fdfind" "--color=never" "--full-path")))
      (consult-find dir)))
  :config
  ;; better preview
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult-buffer
   consult--source-recent-file
   consult--source-project-recent-file
   consult--source-bookmark
   :preview-key "M-.")

  (setq consult-fontify-preserve nil)
  (setq consult-async-min-input 2)
  (setq consult-async-refresh-delay 0.15)
  (setq consult-async-input-throttle 0.2)
  (setq consult-async-input-debounce 0.1)
  ;; this is an option to make
  (setq consult-line-start-from-top nil)
  ;; Optionally configure narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  ;; 让fd支持gbk
  (modify-coding-system-alist 'process "fd" '(utf-8 . gbk-dos))
  ;; [consult-fd]
  (defvar consult-fd-args "fd --color=never -i -H -E .git --regex ")

  (defun +consult--fd-builder (input)
    (pcase-let* ((cmd (split-string-and-unquote consult-fd-args))
                 (`(,arg . ,opts) (consult--command-split input))
                 (`(,re . ,hl) (funcall consult--regexp-compiler arg 'extended t)))
      (when re
        (list :command (append cmd
                               (list (consult--join-regexps re 'extended))
                               opts)
              :highlight hl))))

  (defun +consult-fd (&optional dir initial)
    "Search for regexp with fd in DIR with INITIAL input.
The find process is started asynchronously, similar to `consult-grep'.
See `consult-grep' for more details regarding the asynchronous search."
    (interactive "P")
    (let* ((prompt-dir (consult--directory-prompt "Fd" dir))
           (default-directory (cdr prompt-dir)))
      (find-file (consult--find (car prompt-dir) #'+consult--fd-builder initial)))))

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;; provide annotations in minibuffer
(use-package marginalia
  :config
  (marginalia-mode 1)
  (setq marginalia-align 'center))

;; Vertico seems to lag when dealing with a very long list
;; such as while in describe-functions
(use-package vertico
  :bind (:map minibuffer-local-map
              ("M-<DEL>" . my/minibuffer-backward-kill)
              :map vertico-map
              ("M-q" . vertico-quick-insert)) ; use C-g to exit
  :commands vertico-mode
  :init
  ;; Show more candidates
  ;; (setq vertico-count 20)
  ;; Disable dynamic resize, this is too jumpy
  (setq vertico-resize nil)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle nil)
  (vertico-mode 1)
  :config
  (defun my/minibuffer-backward-kill (arg)
    "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a word"
    (interactive "p")
    (if minibuffer-completing-file-name
        ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
        (if (string-match-p "/." (minibuffer-contents))
            (zap-up-to-char (- arg) ?/)
          (delete-minibuffer-contents))
      (backward-kill-word arg)))

  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                    #'consult-completion-in-region
                  #'completion--in-region)
                args)))
  (setq enable-recursive-minibuffers t)
  (setq completion-cycle-threshold 3))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :defer t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))
  :config
  (setq orderless-matching-styles
        '(orderless-literal
          orderless-regexp)))

;; (grep-apply-setting
;;  'grep-find-command
;;  '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27))

;; (lookup-key vertico-map (kbd "M-q"))
(defun completing--replace-search (search)
  "Replace the SEARCH with string from `read-string'."
  (if (string-empty-p search)
      (user-error "Enter some text first!")
    (let ((to-string (read-string "To: "))
          (buf (seq-find (lambda (x) (not (minibufferp x)))
			             (buffer-list))))
      (with-selected-window (get-buffer-window buf)
        (with-current-buffer buf
	      (query-replace-regexp search to-string)
	      (exit-minibuffer))))))

;;;###autoload
(defun selectsel--yank-search (regex-str)
  "Set search item as str.
Argument REGEX-STR the regex str to find in buffer."
  (when regex-str
    (re-search-forward regex-str (line-end-position) t)
    (if (bound-and-true-p evil-mode)
	(save-excursion
          (evil-search regex-str t t (line-beginning-position)))
      (isearch-mode t)
      (isearch-yank-string regex-str))))

(defun completing-swiper ()
  "My swiper, which also can record macro at end of search."
  (interactive)
  (consult-line (util/thing-at-point/deselect))
  (selectsel--yank-search (car consult--line-history))
  (evil-record-macro ?0))

(defun completing-all-swiper ()
  "My version of swiper."
  (interactive)
  (consult-line-multi 'project (util/thing-at-point/deselect))
  (selectsel--yank-search (car consult--line-history)))

(defun my/consult-line-backward ()
  "Search for a matching line backward."
  (interactive)
  (advice-add 'consult--line-candidates :filter-return 'reverse)
  (unwind-protect
    (consult-line (util/thing-at-point/deselect))
    (advice-remove 'consult--line-candidates 'reverse))
  (selectsel--yank-search (car consult--line-history))
  (evil-record-macro ?0))

(global-set-key (kbd "C-s") 'completing-swiper)
(global-set-key (kbd "C-S-s") 'my/consult-line-backward)

(defun completing-ripgrep (arg)
  "My version of ripgrep.
ARG can be used to control the behaviour of `consult-ripgrep'
A single `universal-argument' can disable preview.
Two `universal-argument' to change read a different directory to ripgrep."
  (interactive "P")
  (require 'consult)
  (let ((init-input (util/thing-at-point/deselect))
        ;; back two variables
        (consult-preview-key consult-preview-key)
        (default-directory default-directory))
    (when (consp arg)
      (cond ((equal arg '(4))
             (setq consult-preview-key nil))
            ((equal arg '(16))
             (setq default-directory
                   (read-file-name
                    "Directory: "
                    default-directory nil nil nil #'file-directory-p)))))
    (consult-ripgrep default-directory init-input))
  (selectsel--yank-search (car consult--grep-history)))

(defun completing-packages (arg &optional filter)
 "List packages, or package archive if ARG non-nil.
use FILTER predicate to filter desired packages to see."
  (interactive (list current-prefix-arg))
  (let* ((packages
          (if arg ; use archive instead
              (mapcar #'car package-archive-contents)
            (append (mapcar #'car package-alist)
                    (mapcar #'car package--builtins))))
         (packages (if (and filter (functionp filter))
                       (-filter filter packages)
                     packages))
         (package (completing-read "Package: " packages)))
    (describe-package (intern package))))

(defun list-installed-themes ()
  "List all installed themes."
  (interactive)
  (completing-packages
   nil
   (lambda (pkg)
     (string-match-p "-themes?$" (symbol-name pkg)))))

;; Enable Corfu completion UI
;; See the Corfu README for more configuration tips.
(use-package corfu
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("C-d" . corfu-info-documentation)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)
        ("C-j" . corfu-insert)
        ("C-u" . corfu-reset)
        ("S-SPC" . corfu-insert-separator)
        ("S-TAB" . corfu-previous)
        ("M-." . corfu-info-location)
        ([?\r] . newline)
        ([backtab] . corfu-previous))
  :hook (after-init . global-corfu-mode)
        (lsp-completion-mode . corfu-lsp-setup)
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 0)
  (corfu-auto-delay 0.05)
  (corfu-echo-documentation 0.3)
  (corfu-quit-no-match 'separator)        ;; Automatically quit if there is no match
  (corfu-preselect-first nil)    ;; Disable candidate preselection
  (corfu-on-exact-match 'quit)
  :preface
  (defun corfu-lsp-setup ()
        (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :config
  (advice-add #'keyboard-quit :before #'corfu-quit)
  (add-to-list 'corfu-auto-commands 'end-of-visual-line)
  ;; https://github.com/minad/corfu/issues/12#issuecomment-869037519
  (with-eval-after-load 'evil
    (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
    (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
    (evil-make-overriding-map corfu-map)
    (evil-define-key 'normal corfu-map (kbd "RET") 'corfu-insert)
    (evil-define-key 'insert corfu-map (kbd "RET") 'corfu-insert))

  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (define-key corfu-map (kbd "RET") 'corfu-insert)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1))

(use-package kind-all-the-icons
  :ensure nil
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters
                #'kind-all-the-icons-margin-formatter))

(use-package corfu-history
  :after corfu
  :commands corfu-history-mode
  :init
  (corfu-history-mode 1)
  :config
  (with-eval-after-load 'savehist
    (cl-pushnew 'corfu-history savehist-additional-variables))
  )

(use-package corfu-terminal
  :when (not (display-graphic-p))
  :commands corfu-terminal-mode 
  :after corfu
  :init (corfu-terminal-mode 1)
  )

;; more at
;; https://kristofferbalintona.me/posts/cape/
(use-package cape
  :defer t
  :hook
  (emacs-lisp-mode . kb/cape-capf-setup-elisp)
  (lsp-completion-mode . vd/setup-lsp-completion)
  (c-mode-common . kb/cape-capf-setup-c)
  (c-ts-mode . kb/cape-capf-setup-c)
  (prog-mode . cape-capf-setup-common)
  :bind (("M-\\" . completion-at-point) ;; default key:  M-tab
         ("C-c p d" . cape-dabbrev)
         ("C-c p f" . cape-file)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p k" . cape-keyword)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict))
  :functions cape-super-capf
  :preface
  (defun vd/setup-lsp-completion ()
    (setq-local completion-at-point-functions (list (cape-super-capf #'lsp-completion-at-point
                                                                     #'cape-keyword
                                                                     #'cape-abbrev))))
  ;; Elisp
  (defun kb/cape-capf-setup-elisp ()
    "Replace the default `elisp-completion-at-point'
completion-at-point-function. Doing it this way will prevent
disrupting the addition of other capfs (e.g. merely setting the
variable entirely, or adding to list).
Additionally, add `cape-file' as early as possible to the list."
    (setf (elt (cl-member 'elisp-completion-at-point completion-at-point-functions) 0)
          #'elisp-completion-at-point)
    (add-to-list 'completion-at-point-functions #'cape-symbol)
    ;; I prefer this being early/first in the list
    (add-to-list 'completion-at-point-functions #'cape-file))

  (defun kb/cape-capf-setup-c ()
      (let (result)
        (dolist (element (list (cape-super-capf #'cape-keyword
                                                #'cape-dabbrev
                                                #'citre-completion-at-point))
                        result)
          (add-to-list 'completion-at-point-functions element))))

  (defun cape-capf-setup-common ()
    (let ((result))
      (dolist (element '(cape-dabbrev cape-file) result)
        (add-to-list 'completion-at-point-functions element))))
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(provide 'init-completing)
;;; init-completing.el ends here
