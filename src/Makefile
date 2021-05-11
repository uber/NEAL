include Makefile.propagate

register:
	SELF_DIR = $(pwd)
	opam exec -- pushd $(SELF_DIR) && ocamlfind remove neal
	opam exec -- pushd $(SELF_DIR) && ocamlfind install neal \
		opam/META \
		core/_build/neal.cmi \
		core/_build/driver/fs.cmo \
		core/_build/driver/fs.cmi \
		core/_build/provider/prabsyn.cmo \
		core/_build/provider/prabsyn.cmi

