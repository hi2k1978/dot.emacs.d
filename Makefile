SUBDIRS=$(shell ls -d */)
.PHONY: all clean  $(SUBDIRS)
all:
	@echo "make all: do nothing"

clean: $(SUBDIRS)
	@\rm -rf \#* *~ \.\#*

$(SUBDIRS):
	$(MAKE) -C $@ $(MAKECMDGOALS)
