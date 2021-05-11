LIBRARY = swift
LIBRARY_EXPORT_PREFIX = provider

include ../../Makefile.build

register:
	make
	NATIVE=1 make
	opam exec -- ocamlfind remove neal-provider-swift
	opam exec -- ocamlfind install neal-provider-swift \
		opam/META \
		_build/parsing/parser.cmi \
		_build/parsing/parser.cmo \
		_build/parsing/combinators.cmo
