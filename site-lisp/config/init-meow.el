(require 'meow)
(meow-global-mode 1)

(setq meow-use-clipbaord  t
      meow-use-keypad-when-execute-kbd nil
      meow-expand-exclude-mode-list nil
      meow-replace-state-name-list '((normal . "N")
                                          (motion . "M")
                                          (keypad . "K")
                                          (insert . "I")
                                          (beacon . "B"))
      meow-use-enhanced-selection-effect t
      meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
      meow-keypad-start-keys '((?c . ?c)
                                    (?x . ?x))
)

(defun lazy-meow-leader-define-key (&rest keybinds)
  (let* ((meow-leader-keybinds))
    (dolist (ele  keybinds)
      (let ((func (cdar ele))
	    (key (caar ele))
	    (filename (cadr ele)))
	(autoload func filename nil t)
	(meow-define-keys 'leader (cons key func))))))

(defun meow-setup ()
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore)
   '("." . repeat))
  
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("t" . one-key-menu-toggle)
   '("f" . one-key-menu-file)
   '("s" . one-key-menu-search)
   '("b" . one-key-menu-buffer)
   '("d" . one-key-menu-code)
   '("u" . one-key-menu-useful)
  ;;  '("g" . one-key-menu-nagivator)
   '("p" . one-key-menu-project)
   '("v" . one-key-menu-git)
   '("o" . one-key-menu-org)
   '("q" . one-key-menu-quit)
   '("w" . one-key-menu-window)
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   '("SPC" . project-find-file)
   '("-" . lsp-bridge-jump-to-next-diagnostic)
   )

  (lazy-meow-leader-define-key
   '(("p" . one-key-menu-project)  "init-project")
   '(("e" . one-key-menu-eaf) "init-eaf")
   '(("n" . one-key-menu-blog) "init-org")
   '(("F" . one-key-menu-fold) "init-vimish-fold")
   '(("r" . +sky/project-recentf) "init-project")
   )
  
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)

   '("a" . meow-append)
   '("A" . meow-append-vim)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-kill)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . one-key-menu-nagivator)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-insert-vim)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . consult-register-store)
   '("M" . meow-block)
   '("n" . meow-search)
   '("N" . meow-pop-selection);;

   '("o" . meow-open-below)
   '("O" . meow-open-above)
   '("p" . meow-yank)
   '("P" . meow-yank-pop);;
   '("q" . meow-quit)
   '("Q" . consult-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-line)
   '("S" . meow-kmacro-lines) ;;
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . vundo)
   '("v v" . meow-visit)
   '("V" . meow-kmacro-matches) ;;
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)

   '("x" . meow-delete)
   '("X" . meow-backward-delete)
   '("y" . meow-save)
  ;;  '("Y" . meow-sync-grab)
    '("z a" . hs-toggle-hiding)
    '("z c" . hs-hide-block)
    '("z o" . hs-show-block)
    '("z m" . hs-hide-all)
    '("z r" . hs-show-all)

    '("v i" . meow-inner-of-thing)
    '("v a" . meow-bounds-of-thing)
    '("v \'" . insert-quotations)
    '("v \"" . insert-quotes)
    '("v \`" . insert-backquote)
    '("v *" . insert-star)
    '("v (" . insert-parentheses)
    '("v [" . insert-bracket)
    '("v {" . insert-curly)
    '("v =" . insert-equation)

    '("-" . negative-argument)
    ;; '("=" . format-all-region)
    '("=" . indent-region)
    '("[" . meow-beginning-of-thing)
    '("]" . meow-end-of-thing)
    '("\\" . quoted-insert)
    '(";" . meow-expand-1)
    '(":" . async-shell-command)
    '("'" . consult-register-load)
    '("," . meow-reverse)
    '("." . repeat)

    '("<escape>" . ignore)
    '("!" . meow-start-kmacro-or-insert-counter)
    '("@" . meow-end-or-call-kmacro)
    '("#" . symbol-overlay-put)
    '("^" . meow-join)
    '("*" . symbol-overlay-put)
    '("/" . consult-line)
    ))

(defun meow-append-vim()
  (interactive)
  (progn (meow-line 1)
         (meow-append)))

(defun meow-insert-vim()
  (interactive)
  (progn (meow-join 1)
         (meow-append)))

(meow-setup)
(provide 'init-meow)
