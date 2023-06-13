emacs.el:
	@emacs -Q --batch -l early-init.el -l lisp/tangle.el

%.elc: %.el
	@emacs -Q -l early-init.el --batch --eval '(byte-compile-file "$<")'

all: init.elc emacs.elc early-init.elc

.PHONY: clean

clean:
	@rm -f init.elc emacs.el emacs.elc

