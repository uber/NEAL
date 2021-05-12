SRC_DIR = $(shell pwd)


include Makefile.propagate

register:
	opam exec -- ocamlfind remove neal
	opam exec -- ocamlfind install neal \
		$(SRC_DIR)/opam/META \
		$(SRC_DIR)/core/_build/neal.cmi \
		$(SRC_DIR)/core/_build/driver/fs.cmo \
		$(SRC_DIR)/core/_build/driver/fs.cmi \
		$(SRC_DIR)/core/_build/provider/prabsyn.cmo \
		$(SRC_DIR)/core/_build/provider/prabsyn.cmi

