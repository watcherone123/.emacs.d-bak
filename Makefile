-include lib/borg/borg.mk

SHELL = /bin/sh
EMACS ?= emacs
INITFILE = ~/.config/emacs/init.el
EMACSPATH = ~/.config/emacs

.PHONY: all time test

.DEFAULT_GOAL := all

time:
	$(EMACS) --batch -q --eval '(message "%s" (emacs-init-time))'

bootstrap-borg:
	@git submodule--helper clone --name borg --path lib/borg \
	--url https://github.com/emacscollective/borg.git
	@cd lib/borg; git symbolic-ref HEAD refs/heads/main
	@cd lib/borg; git reset --hard HEAD

# 初始化下载，更新到 .gitmodules 中指定的 commit
init:
	@git submodule update --init --recursive
	@git submodule foreach git reset --hard
	@git submodule foreach 'git checkout master || git checkout main'

# 修改 .gitmodules 后
sync:
	git submodule sync

update:
	git submodule foreach git pull --rebase
	
# Run tests.
test:
	@$(EMACS) -Q -nw --batch -l init.el --eval '(progn (memory-report) (message (buffer-string)))'
	@$(EMACS) -nw --batch -l init.el --eval '(message "startup time: %s, gcs-done=%d" (emacs-init-time) gcs-done)'
# -l tests/emacs.d-test.el
