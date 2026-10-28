EMACS ?= emacs
# LispM Kit, with which the windows declare their commands: a checkout
# beside this one by default.
LISPM_KIT ?= ../lispm-kit
LOAD_PATH = -L . -L $(LISPM_KIT)

FILES = lispm-mail-worker.el lispm-mail-style.el lispm-mail-accounts.el lispm-mail-data.el lispm-mail-sign-in.el lispm-mail-view.el \
	lispm-mail-compose.el lispm-mail-settings.el lispm-mail.el

.PHONY: test compile secrets

test:
	$(EMACS) -Q --batch $(LOAD_PATH) --eval '(setq native-comp-enable-subr-trampolines nil)' \
	  -l test/lispm-mail-test.el -f ert-run-tests-batch-and-exit

# A check: the .elc files are removed again, as they would shadow the
# sources of a checkout Emacs loads from.
compile:
	$(EMACS) -Q --batch $(LOAD_PATH) --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(FILES); \
	status=$$?; rm -f *.elc; exit $$status

# The file of secrets a release comes with, `lispm-mail-secrets-file',
# from the environment: GOOGLE_CLIENT_SECRET, the secret of
# `lispm-mail-google-client-id'.  It is never committed.  The form is a
# variable: a recipe would hand its line ends, quoted, to the shell.
SECRETS = (let ((secret (string-trim (or (getenv "GOOGLE_CLIENT_SECRET") "")))) \
	    (when (string-empty-p secret) (error "GOOGLE_CLIENT_SECRET is not set")) \
	    (with-temp-file lispm-mail-secrets-file \
	      (prin1 `((google :client-id ,lispm-mail-google-client-id :client-secret ,secret)) \
	             (current-buffer)) \
	      (insert "\n")))

secrets:
	$(EMACS) -Q --batch -L . -l lispm-mail-accounts --eval '$(SECRETS)'
