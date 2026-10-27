EMACS ?= emacs
# LispM Kit, with which the windows declare their commands: a checkout
# beside this one by default.
LISPM_KIT ?= ../lispm-kit
LOAD_PATH = -L . -L $(LISPM_KIT)

FILES = lispm-mail-worker.el lispm-mail-style.el lispm-mail-accounts.el lispm-mail-data.el lispm-mail-view.el \
	lispm-mail-compose.el lispm-mail-settings.el lispm-mail.el

.PHONY: test compile

test:
	$(EMACS) -Q --batch $(LOAD_PATH) --eval '(setq native-comp-enable-subr-trampolines nil)' \
	  -l test/lispm-mail-test.el -f ert-run-tests-batch-and-exit

# A check: the .elc files are removed again, as they would shadow the
# sources of a checkout Emacs loads from.
compile:
	$(EMACS) -Q --batch $(LOAD_PATH) --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(FILES); \
	status=$$?; rm -f *.elc; exit $$status
