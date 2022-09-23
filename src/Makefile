#SUBDIRS=$(shell ls -d */)

.PHONY: all clean byte-compile emacs kill $(SUBDIRS)

all:
	@echo "this is a collections of my sample progpams written by C++"

clean: $(SUBDIRS)
	@\rm -rf \#* *~ \.\#* *.elc

$(SUBDIRS):
	$(MAKE) -C $@ $(MAKECMDGOALS)

byte-compile:
	@echo byte-compile ~/.emacs.d/init.el
	@emacs --batch -f batch-byte-compile init.el
emacs:
	@\emacs init.el

kill:
	@echo process kill emacs
	@pkill emacs
