MAIN_FILE = main

include ../Makefile.build

build::
	-@opam exec -- ocamlfind remove neal &> /dev/null
	-@$(MAKE) -C $(SELF_DIR)/.. register &> /dev/null
ifneq ($(COVERAGE),)
	@[ -f main.$(EXEC_EXT) ] && mv main.$(EXEC_EXT) main.cov.$(EXEC_EXT) || true
else
	@[ -f main.cov.$(EXEC_EXT) ] && mv main.cov.$(EXEC_EXT) main.$(EXEC_EXT) || true
endif
