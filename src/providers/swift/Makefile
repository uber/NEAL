LIBRARY = swift
LIBRARY_EXPORT_PREFIX = provider
SWIFT_DIR = $(shell pwd)

include ../../Makefile.build

register:
	make
	NATIVE=1 make
	opam exec -- ocamlfind remove neal-provider-swift
	opam exec -- ocamlfind install neal-provider-swift \
		$(SWIFT_DIR)/opam/META \
		$(SWIFT_DIR)/_build/parsing/parser.cmi \
		$(SWIFT_DIR)/_build/parsing/parser.cmo \
		$(SWIFT_DIR)/_build/parsing/combinators.cmo
