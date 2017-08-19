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

# Codeship PRE
# npm install emacs
# ln -sf /home/rof/cache/emacs/bin/emacs ~/bin/
# curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
# export PATH="/home/rof/.cask/bin:$PATH"

# Codeship POST
# mkdir -p .cask/$(emacs --version | head -1 | awk -v N=3 '{print $N}')
# make all

# pip install emacs
# Collecting emacs
#   Downloading emacs-24.1.50.1-20120610.tar.gz
# Building wheels for collected packages: emacs
#   Running setup.py bdist_wheel for emacs ... done
#   Stored in directory: /home/rof/cache/pip/wheels/e2/1f/d4/74c9fb56f2db93ecc3442caeff57e2a5eb52b53b467224f40a
# Successfully built emacs
# Installing collected packages: emacs
# Successfully installed emacs-24.1.50.1.post20120610
