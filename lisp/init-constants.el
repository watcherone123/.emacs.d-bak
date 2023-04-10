;;; init-constants.el --- Locations of everything

;;; Commentary:
;;

;;; Code:

(defconst system-init-time (current-time)
  "When Emacs starting evaling our code.
Similar to `before-init-time'")

(defvar shadow-site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory)
  "The home of my site-lisp functionality.")
(add-to-list 'load-path shadow-site-lisp-dir)

(defvar shadow-cache-dir (expand-file-name ".cache" user-emacs-directory)
  "This directory for cache files.")

;;--------------------------------------------
(defconst Shadow-homepage
  "https://github.com/e190/emacs-config"
  "The Github page of Centuar Emacs.")

(defcustom Shadow-logo (expand-file-name "img/dashLogo.png" user-emacs-directory)
  "Set Centaur logo. nil means official logo."
  :type 'string)

(defconst sys/gui
  (display-graphic-p)
  "Are we running on a GUI Emacs?")

(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")

(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(defconst emacs/>=27p
  (>= emacs-major-version 27)
  "Emacs is 27 or above.")

(defconst emacs/>=28p
  (>= emacs-major-version 28)
  "Emacs is 28 or above.")

(defconst emacs/>=29p
  (>= emacs-major-version 29)
  "Emacs is 29 or above.")

(defconst emacs/>=29.1p
  (or (> emacs-major-version 29)
      (and (= emacs-major-version 29) (>= emacs-minor-version 1)))
  "Emacs is 29.1 or above.")

(defconst *rg*
  (executable-find "rg")
  "Do we have ripgrep?")

(defconst *fd*
  (executable-find "fd")
  "Do we have fd")

(defconst *git*
  (executable-find "git")
  "Do we have git?")
;;------------------------------------------------------
(defcustom shadow-mail-address "e190@163.com"
  "Default email address."
  :group 'shadow
  :type 'string)

(defcustom centaur-completion-style 'minibuffer
  "Completion display style."
  :group 'centaur
  :type '(choice (const :tag "Minibuffer" minibuffer)
                 (const :tag "Child Frame" childframe)))

(defcustom shadow-lsp-mode 'lsp-mode
 "Set language server."
  :group 'shadow
  :type '(choice
         (const :tag "LSP Mode" 'lsp-mode)
         (const :tag "eglot" 'eglot)
         (const :tag "ctags" 'ctags)
         (const :tag "gtags" 'gtags)
         nil))

(defcustom sniper-server nil
  "Enable `server-mode' or not."
  :group 'centaur
  :type 'boolean)

(defcustom use-rime nil
  "Enable `rime' or not."
  :group 'centaur
  :type 'boolean)

(defcustom sniper-font '("Fira Code" . 11)
  "Set font."
  :group 'centaur
  :type '(alist :key-type string :value-type number))

(provide 'init-constants)
;;; init-constants.el ends here
