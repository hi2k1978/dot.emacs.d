SUBDIRS=$(shell ls -d */)
DEST=~/.emacs.d/init.el
SRC=./init.el
.PHONY: all clean  $(SUBDIRS) replace
all:
	@echo "this is a collections of my sample progpams written by C++"
clean: $(SUBDIRS)
	@\rm -rf \#* *~ \.\#*

$(SUBDIRS):
	$(MAKE) -C $@ $(MAKECMDGOALS)

replace:
	@echo "copy file: $(SRC) -> $(DEST)"
	@cp $(SRC) $(DEST)
