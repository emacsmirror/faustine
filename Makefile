EMACS = emacs

all: README.md

README.md: ../make-readme-markdown/make-readme-markdown.el
	$(EMACS) --script $< <faustine.el $< > $@

clean:
	rm -f *.elc

.PHONY: README.md
