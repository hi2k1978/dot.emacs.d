#SUBDIRS=$(shell ls -d */)

.PHONY: all clean build run stop $(SUBDIRS)

all:
	@echo "this is a collections of my sample progpams written by C++"

clean: $(SUBDIRS)
	@\rm -rf \#* *~ \.\#* *.elc

$(SUBDIRS):
	$(MAKE) -C $@ $(MAKECMDGOALS)

build:
	@echo byte-compile ~/.emacs.d/init.el
	@emacs --batch -f batch-byte-compile init.el
run:
	@\emacs init.el

stop:
	@echo process kill emacs
	@pkill emacs
