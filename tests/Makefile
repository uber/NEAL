include ../src/Makefile.build

ifdef FILTER
	F:=--filter=$(FILTER)
endif

test::
	source "$(SELF_DIR)/../env/bin/activate"; \
	NEAL=$(NEAL) lit -v $(F) .
