include ../src/Makefile.build

ifdef FILTER
	F:=--filter=$(FILTER)
endif

test::
	. "$(SELF_DIR)/../env/bin/activate"; \
	NEAL=$(NEAL) opam exec -- lit -v $(F) .
