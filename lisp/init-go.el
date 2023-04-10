;; init-go.el --- Initialize Golang configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Golang configurations.
;;

;;; Code:

;; Golang
(use-package go-mode
  :functions go-update-tools
  :commands godoc-gogetdoc
  :bind (:map go-mode-map
         ("C-c R" . go-remove-unused-imports)
         ("<f1>" . godoc-at-point))
  :init (setq godoc-at-point-function #'godoc-gogetdoc)
  :config
  (setenv "GOPATH" (concat (file-truename "/home/sniper/") "go/"))
  (setenv "PATH"
          (concat (getenv "PATH") ":" (concat (getenv "GOPATH") "bin")))
  (setenv "GO111MODULE" "on")
  (setenv "GOPROXY" "https://goproxy.io")
  (set-default 'godef-command (concat (getenv "GOPATH") "bin/godef"))
  (unless (executable-find "gopls")
    (message "Unable to find `gopls' in `exec-path'!"))
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY")))

  ;; Install or update tools
  (defvar go--tools '("golang.org/x/tools/cmd/goimports"
                      "github.com/go-delve/delve/cmd/dlv"
                      "github.com/zmb3/gogetdoc"
                      "github.com/josharian/impl"
                      "github.com/cweill/gotests/..."
                      "github.com/fatih/gomodifytags"
                      "github.com/davidrjenni/reftools/cmd/fillstruct")
    "All necessary go tools.")

  ;; Do not use the -u flag for gopls, as it will update the dependencies to incompatible versions
  ;; https://github.com/golang/tools/tree/master/gopls#installation
  (defvar go--tools-no-update '("golang.org/x/tools/gopls@latest")
    "All necessary go tools without update the dependencies.")

  (defun go-update-tools ()
    "Install or update go tools."
    (interactive)
    (unless (executable-find "go")
      (user-error "Unable to find `go' in `exec-path'!"))

    (message "Installing go tools...")

    ;; https://github.com/golang/tools/tree/master/gopls#installation
    (async-shell-command
     "GO111MODULE=on go get golang.org/x/tools/gopls@latest")

    ;; https://staticcheck.io/docs/install
    (async-shell-command
     "go install honnef.co/go/tools/cmd/staticcheck@latest")

    (dolist (pkg go--tools)
      (set-process-sentinel
       (start-process "go-tools" "*Go Tools*" "go" "get" "-u" "-v" pkg)
       (lambda (proc _)
         (let ((status (process-exit-status proc)))
           (if (= 0 status)
               (message "Installed %s" pkg)
             (message "Failed to install %s: %d" pkg status)))))))

  ;; Try to install go tools if `gopls' is not found
  (unless (executable-find "gopls")
    (go-update-tools))

  ;; Misc
  (use-package go-dlv)
  (use-package go-fill-struct)
  (use-package go-impl)

  ;; Install: See https://github.com/golangci/golangci-lint#install
  (use-package flycheck-golangci-lint
    :if (executable-find "golangci-lint")
    :after flycheck
    :defines flycheck-disabled-checkers
    :hook (go-mode . (lambda ()
                       "Enable golangci-lint."
                       (setq flycheck-disabled-checkers '(go-gofmt
                                                          go-golint
                                                          go-vet
                                                          go-build
                                                          go-test
                                                          go-errcheck))
                       (flycheck-golangci-lint-setup))))

  (use-package go-tag
    :bind (:map go-mode-map
           ("C-c t t" . go-tag-add)
           ("C-c t T" . go-tag-remove))
    :init (setq go-tag-args (list "-transform" "camelcase")))

  (use-package go-gen-test
    :bind (:map go-mode-map
           ("C-c t g" . go-gen-test-dwim)))

  (use-package gotest
    :bind (:map go-mode-map
           ("C-c t a" . go-test-current-project)
           ("C-c t m" . go-test-current-file)
           ("C-c t ." . go-test-current-test)
           ("C-c t x" . go-run))))

;; Local Golang playground for short snippets
(use-package go-playground
  :diminish
  :commands (go-playground-mode))

(provide 'init-go)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-go.el ends here
