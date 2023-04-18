;;; init-key.el --- Generic config. -*- lexical-binding: t -*-
(require 'general)
(require 'init-funcs)

(global-set-key (kbd "C-x C-r") #'restart-emacs)

(lazy-one-key-create-menu "Toggle"
                          (:key "F" :description "Toggle bit font" :command +sky/toggle-big-font :filename "init-font")
                          (:key "p" :description "Toggle proxy" :command +sky/toggle-proxy :filename "init-emacs")
                          (:key "l" :description "Toggle line numbers" :command display-line-numbers-mode :filename "display-line-numbers")
                          (:key "c" :description "Toggle crow" :command crow-mode :filename "init-crow")
                          (:key "s" :description "Toggle eshell" :command eshell :filename "eshell")
                          (:key "T" :description "Toggle transparent" :command +sky/toggle-transparent :filename "init-emacs")
                          (:key "f" :description "Toggle dirvish side" :command +sky/dirvish-side-current-path :filename "init-dirvish"))

(lazy-one-key-create-menu "Quit"
                          (:key "q" :description "Quit Emacs save session" :command emacs-session-save :filename "init-session")
                          )

(lazy-one-key-create-menu "File"
                          (:key "s" :description "Save buffer" :command save-buffer :filename "")
                          (:key "d" :description "Drivish" :command dirvish :filename "init-dirvish")
                          (:key "e" :description "Rename file" :command rename-this-file :filename "")
                          (:key "p" :description "Find emacs config" :command +sky/find-emacs-config :filename "init-emacs")
                          (:key "r" :description "Find recent file" :command consult-recent-file :filename "init-consult")
                          (:key "f" :description "Find file" :command find-file :filename "")
                          )

(lazy-one-key-create-menu "Search"
                          (:key "s" :description "Search in buffer" :command consult-line :filename "init-consult")
                          (:key "S" :description "Search in multi buffer" :command consult-imenu-multi
                                :filename "init-consult")
                          (:key "g" :description "RipGreep here" :command consult-ripgrep :filename "init-consult")
                          (:key "f" :description "GitGrep here" :command consult-git-grep :filename "init-consult")
                          (:key "b" :description "Blink Search" :command blink-search :filename "init-blink-search")
                          (:key "r" :description "Colorg" :command one-key-menu-color-rg :filename "init-color-rg")
                          (:key "y" :description "Kill ring history" :command consult-yank-pop :filename "init-consult")
                          (:key "e" :description "Fanyi" :command fanyi-dwim :filename "init-fanyi")
                          (:key "B" :description "Bookmark" :command consult-bookmark :filename "init-consult")
                          )

(lazy-one-key-create-menu "Buffer"
                          (:key "b" :description "Switch buffers" :command consult-buffer :filename "init-consult")
                          (:key "k" :description "Kill buffer" :command kill-buffer-and-window :filename "")
                          (:key "r" :description "Revert buffer" :command revert-buffer :filename "")
                          (:key "t" :description "switch tab" :command one-key-menu-sort-tab :filename "init-sort-tab")
                          )

(lazy-one-key-create-menu "Nagivator"
                          (:key "f" :description "Format code" :command apheleia-format-buffer :filename "init-format")
                          (:key "d" :description "find-definitions" :command xref-find-definitions :filename "")
                          (:key "D" :description "find-def-other-window" :command xref-find-definitions-other-window :filename "")
                          (:key "r" :description "find-references" :command xref-find-references :filename "")
                          (:key "j" :description "find-apropos" :command xref-find-apropos :filename "")
                          (:key "n" :description "Lsp Bridge rename" :command lsp-bridge-rename :filename "init-lsp-bridge")
                          (:key "i" :description "Lsp Bridge find impl" :command lsp-bridge-find-impl :filename "init-lsp-bridge")
                          (:key "s" :description "Lsp Bridge show document" :command lsp-bridge-lookup-documentation :filename "init-lsp-bridge")
                          (:key "s" :description "Lsp Bridge code action" :command lsp-bridge-code-action :filename "init-lsp-bridge")
                          (:key "E" :description "Lsp Bridge toggle diagnostics" :command lsp-bridge-toggle-diagnostics :filename "init-lsp-bridge")
                          )

(lazy-one-key-create-menu "Code"
                          (:key "f" :description "Format code" :command apheleia-format-buffer :filename "init-format")
                          (:key "e" :description "Lsp Bridge Diagnostic" :command one-key-menu-diagnostic :filename "init-lsp-bridge")
                          (:key "d" :description "Lsp Bridge jump to def" :command lsp-bridge-find-def :filename "init-lsp-bridge")
                          (:key "D" :description "Lsp Bridge jump to def other window" :command lsp-bridge-find-def-other-window :filename "init-lsp-bridge")
                          (:key "b" :description "Lsp Bridge jump back" :command lsp-bridge-find-def-return :filename "init-lsp-bridge")
                          (:key "r" :description "Lsp Bridge find reference" :command lsp-bridge-find-references :filename "init-lsp-bridge")
                          (:key "n" :description "Lsp Bridge rename" :command lsp-bridge-rename :filename "init-lsp-bridge")
                          (:key "i" :description "Lsp Bridge find impl" :command lsp-bridge-find-impl :filename "init-lsp-bridge")
                          (:key "s" :description "Lsp Bridge show document" :command lsp-bridge-lookup-documentation :filename "init-lsp-bridge")
                          (:key "s" :description "Lsp Bridge code action" :command lsp-bridge-code-action :filename "init-lsp-bridge")
                          (:key "E" :description "Lsp Bridge toggle diagnostics" :command lsp-bridge-toggle-diagnostics :filename "init-lsp-bridge")
                          (:key ";" :description "comment code" :command comment-or-uncomment-region+ :filename "duplicate-line")
                          )

(lazy-one-key-create-menu "EAF"
                          (:key "o" :description "EAF Open anything" :command eaf-open  :filename "init-eaf")
                          (:key "b" :description "EAF Open browser" :command eaf-open-browser  :filename "init-eaf")
                          (:key "h" :description "EAF Open browser with history" :command eaf-open-browser-with-history  :filename "init-eaf")
                          (:key "s" :description "EAF Search" :command eaf-search-it  :filename "init-eaf")
                          )

;; (lazy-one-key-create-menu "Magit"
;;                           (:key "v" :description "Open Magit" :command magit :filename "init-magit")
;;                           (:key "l" :description "Yank git link with current line." :command git-link :filename "git-link")
;;                           )

(lazy-one-key-create-menu "Useful"
                          (:key "u" :description "Translate region" :command gts-do-translate :filename "init-go-translate")
                          (:key "c" :description "Crow mode" :command one-key-menu-crow :filename "init-crow")
                          (:key "e" :description "English helper" :command lsp-bridge-toggle-english-helper :filename "init-lsp-bridge")
                          (:key "p" :description "Yank buffer filename" :command +sky/yank-buffer-file-name :filename "")
                          (:key "S" :description "Sudo edit" :command sudo-edit :filename "init-sudo-edit")
                          (:key "1" :description "Profiler start" :command profiler-start :filename "profiler")
                          (:key ";" :description "popweb-dict-bing" :command popweb-dict-bing-input :filename "init-popweb")
                          (:key "y" :description "translate-and-mark-unknown-word" :command popweb-translate-and-mark-unknown-word :filename "init-popweb")
                          )

(lazy-one-key-create-menu "Org"
                          (:key "c" :description "Org capture" :command org-capture :filename "init-org")
                          (:key "a" :description "Org agenda" :command org-agenda :filename "init-org"))


(lazy-load-local-keys '(("C-x u" . vundo))
                      global-map "init-vundo")

;; (lazy-load-global-keys '(("M-x" . counsel-M-x))
;;             "init-ivy")

(lazy-load-global-keys '(("C-x m" . embark-act))
                       "init-embark")
(require 'which-key)
(run-with-idle-timer 2 nil #'which-key-mode)
(with-eval-after-load 'which-key
                           
                            
                              (setq which-key-idle-delay 0.3
        which-key-compute-remaps t
        which-key-min-display-lines 1
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-sort-uppercase-first nil
        which-key-side-window-max-width 0.33
        which-key-side-window-max-height 0.20
        which-key-sort-order #'which-key-prefix-then-key-order)
         (which-key-setup-side-window-bottom)
           (dolist (item '((("SPC" . nil) . ("␣" . nil))
                  (("TAB" . nil) . ("↹" . nil))
                  (("RET" . nil) . ("⏎" . nil))
                  (("DEL" . nil) . ("⌫" . nil))
                  (("<up>" . nil) . ("↑" . nil))
                  (("<down>" . nil) . ("↓" . nil))
                  (("<left>" . nil) . ("←" . nil))
                  (("<right>" . nil) . ("→" . nil))
                  (("deletechar" . nil) . ("⌦" . nil))
                  ;; rename winum-select-window-1 entry to 1..9
                  (("\\(.*\\)1" . "winum-select-window-1") . ("\\11..9" . "window 1..9"))
                  ;; hide winum-select-window-[2-9] entries
                  ((nil . "winum-select-window-[2-9]") . t)))
    (cl-pushnew item which-key-replacement-alist :test #'equal))
    (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
                            )

(general-create-definer sky-space-key-define
  :states '(normal visual motion evilified)
  :keymaps 'override
  :prefix "SPC"
  :non-normal-prefix "M-SPC")

(general-create-definer sky-local-key-define
  :states '(normal visual motion evilified)
  :keymaps 'override
  :prefix ",")

(sky-space-key-define
  "SPC" 'execute-extended-command
  ";" 'comment-or-uncomment
  "b" 'one-key-menu-buffer
  "f" 'one-key-menu-file
  "s" 'one-key-menu-search
  "p" 'one-key-menu-project
  "w" 'one-key-menu-window
  "q" 'one-key-menu-quit
)

(provide 'init-key)
