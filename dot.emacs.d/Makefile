SUBDIRS=$(shell ls -d */)
.PHONY: all clean  $(SUBDIRS)
all:
	@echo "this is a collections of my sample progpams written by C++"
clean: $(SUBDIRS)
	@\rm -rf \#* *~ \.\#*

$(SUBDIRS):
	$(MAKE) -C $@ $(MAKECMDGOALS)
