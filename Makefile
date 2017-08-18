.PHONY : tests clr


EMACS ?= emacs
CASK ?= cask

LOADPATH = -L .
# LOADPATH = $(filter-out helpers/doc-a-mode.el, - L .)

ELPA_DIR = \
	.cask/$(shell $(EMACS) -Q --batch --eval '(princ emacs-version)')/elpa

all: clr tests

clr:
	clear

install_cask:
	curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

pre-test:
	rm -f *.elc
	$(CASK) exec $(EMACS) -batch -Q -L . -eval "(progn (setq byte-compile-error-on-warn t) (batch-byte-compile))" faustine.el

tests: elpa pre-test
	$(CASK) exec $(EMACS) -Q -batch $(LOADPATH) \
		$(patsubst %,-l %,$(wildcard test/test-*.el)) \
		-f ert-run-tests-batch-and-exit

test/test-%: elpa pre-test
	$(CASK) exec $(EMACS) -Q -batch $(LOADPATH) \
		-l $@ \
		-f ert-run-tests-batch-and-exit

elpa: $(ELPA_DIR)
$(ELPA_DIR): Cask
	$(CASK) install
	touch $@
