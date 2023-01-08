(global-set-key (kbd "C-x C-r") #'restart-emacs)
(lazy-one-key-create-menu "Toggle"
			  (:key "F" :description "Toggle bit font" :command +sky/toggle-big-font :filename "init-font")
			  (:key "p" :description "Toggle proxy" :command +sky/toggle-proxy :filename "init-emacs")
			  (:key "l" :description "Toggle line numbers" :command display-line-numbers-mode :filename "display-line-numbers")
			  (:key "c" :description "Toggle crow" :command crow-mode :filename "init-crow")
			  (:key "s" :description "Toggle eshell" :command eshell :filename "eshell")
			  (:key "T" :description "Toggle transparent" :command +sky/toggle-transparent :filename "init-emacs")
			  (:key "f" :description "Toggle dirvish side" :command +sky/dirvish-side-current-path :filename "init-dirvish"))

(one-key-create-menu
 "Window"
 '((("l" . "Focus right window") . windmove-right)
   (("h" . "Focus left window") . windmove-left)
   (("k" . "Focus up window") . windmove-up)
   (("j" . "Focus down window") . windmove-down)
   (("L" . "Swap right window") . windmove-swap-states-right)
   (("H" . "Swap left window") . windmove-swap-states-left)
   (("K" . "Swap up window") . windmove-swap-states-up)
   (("J" . "Swap down window") . windmove-swap-states-down)
   (("s" . "Split window vertically") . split-window-below)
   (("v" . "Split window horizontally") . split-window-right)
   (("d" . "Delete window") . delete-window)
   (("u" . "Undo window") . winner-undo)
   (("C-h" . "Resize window to smaller") . shrink-window-horizontally)
   (("m" . "Delete other window") . delete-other-windows)
   (("C-k" . "Scroll other window up") . scroll-other-window-down)
   (("C-j" . "Scroll other window down") . scroll-other-window)))

(lazy-one-key-create-menu "File"
			  (:key "s" :description "Save buffer" :command save-buffer :filename "")
			  (:key "d" :description "Drivish" :command dirvish :filename "init-dirvish")
			  (:key "p" :description "Find emacs config" :command +sky/find-emacs-config :filename "init-ivy")
			  (:key "r" :description "Find recent file" :command counsel-recentf :filename "init-ivy")
			  (:key "f" :description "Find file" :command find-file :filename "init-ivy")
			  (:key "F" :description "Find file with fuzzy" :command counsel-fzf :filename "init-ivy"))

(lazy-one-key-create-menu "Search"
			  (:key "s" :description "Search in buffer" :command swiper :filename "init-ivy")
			  (:key "S" :description "Search in multi buffer" :command swiper-multi
				:filename "init-ivy")
			  (:key "g" :description "RipGreep here" :command counsel-rg :filename "init-ivy")
			  (:key "f" :description "GitGrep here" :command counsel-git-grep :filename "init-ivy")
			  (:key "d" :description "Blink Search" :command blink-search :filename "init-blink-search")
			  (:key "r" :description "Colorg" :command color-rg-search-input :filename "init-color-rg")
			  (:key "y" :description "Kill ring history" :command counsel-yank-pop :filename "init-ivy")
			  (:key "e" :description "Fanyi" :command fanyi-dwim :filename "init-fanyi")
			  (:key "B" :description "Bookmark" :command counsel-bookmark :filename "init-ivy")
			  (:key "l" :description "Find libray" :command counsel-find-library :filename "init-ivy"))

(lazy-one-key-create-menu "Buffer"
			  (:key "b" :description "Switch buffers" :command counsel-switch-buffer :filename "init-ivy")
			  (:key "k" :description "Kill buffer" :command kill-buffer-and-window :filename "")
			  (:key "r" :description "Revert buffer" :command revert-buffer :filename ""))

(one-key-create-menu
 "Nagivator"
 '((("d" . "Go to definetion") . xref-find-definitions)))

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
			  (:key "`" :description "Add Fold code" :command vimish-fold :filename "init-vimish-fold")
			  (:key "~" :description "Delete Fold code" :command vimish-fold-delete :filename "init-vimish-fold"))

(lazy-one-key-create-menu "EAF"
			  (:key "o" :description "EAF Open anything" :command eaf-open  :filename "init-eaf")
			  (:key "b" :description "EAF Open browser" :command eaf-open-browser  :filename "init-eaf")
			  (:key "h" :description "EAF Open browser with history" :command eaf-open-browser-with-history  :filename "init-eaf")
			  (:key "s" :description "EAF Search" :command eaf-search-it  :filename "init-eaf")
			  )

(lazy-one-key-create-menu "Magit"
			  (:key "v" :description "Open Magit" :command magit :filename "init-magit")
			  (:key "l" :description "Yank git link with current line." :command git-link :filename "git-link")
			  )

(lazy-one-key-create-menu "Useful"
			  (:key "u" :description "Translate region" :command gts-do-translate :filename "init-go-translate")
			  (:key "c" :description "Crow mode" :command one-key-menu-crow :filename "init-crow")
			  (:key "e" :description "English helper" :command lsp-bridge-toggle-english-helper :filename "init-lsp-bridge")
			  (:key "p" :description "Yank buffer filename" :command +sky/yank-buffer-file-name :filename "")
			  (:key "S" :description "Sudo edit" :command sudo-edit :filename "init-sudo-edit")
			  (:key "i" :description "Counsel tips" :command one-key-menu-counsel :filename "init-ivy")
			  (:key "1" :description "Profiler start" :command profiler-start :filename "profiler")
			  )

(lazy-one-key-create-menu "Org"
			  (:key "c" :description "Org capture" :command org-capture :filename "init-org")
			  (:key "a" :description "Org agenda" :command org-agenda :filename "init-org"))


(lazy-load-local-keys '(("C-x u" . vundo))
		      global-map "init-vundo")

;; (lazy-load-global-keys '(("M-x" . counsel-M-x))
;; 		       "init-ivy")

(lazy-load-global-keys '(("C-x m" . embark-act))
		       "init-embark")

(provide 'init-key)