#SUBDIRS=$(shell ls -d */)
DEST=~/.emacs.d/init.el
SRC=./init.el

.PHONY: all clean byte-compile kill $(SUBDIRS)

all:
	@echo "this is a collections of my sample progpams written by C++"

clean: $(SUBDIRS)
	@\rm -rf \#* *~ \.\#* *.elc

$(SUBDIRS):
	$(MAKE) -C $@ $(MAKECMDGOALS)

byte-compile:
	@emacs --batch -f batch-byte-compile init.el
kill:
	@pkill emacs
