SHELL = /bin/sh
EMACS ?= emacs
INITFILE = ~/.config/emacs/init.el
EMACSPATH = ~/.config/emacs

.PHONY: all install clean upgrade compile time test

all: install upgrade compile

.DEFAULT_GOAL := all

install:
	@$(EMACS) --batch --load $(INITFILE)

time:
	$(EMACS) --batch -q --eval '(message "%s" (emacs-init-time))'

clean:
	@rm -r $(EMACSPATH)/elpa/*

upgrade:
	@$(EMACS) --batch -nw --load $(INITFILE) --eval '(update-packages)'

compile:
	@rm $(EMACSPATH)/cache/autoloads.pkg.el*
	@$(EMACS) --load $(INITFILE) --eval '(maple-package-force-initialize)'

# Run tests.
test:
	@$(EMACS) -Q -nw --batch -l init.el --eval '(progn (memory-report) (message (buffer-string)))'
	@$(EMACS) -nw --batch -l init.el --eval '(message "startup time: %s, gcs-done=%d" (emacs-init-time) gcs-done)'
# -l tests/emacs.d-test.el
