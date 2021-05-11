include Makefile.propagate

register:
	opam exec --working-dir=$(pwd) -- ocamlfind remove neal
	opam exec --working-dir=$(pwd) -- ocamlfind install neal \
		opam/META \
		core/_build/neal.cmi \
		core/_build/driver/fs.cmo \
		core/_build/driver/fs.cmi \
		core/_build/provider/prabsyn.cmo \
		core/_build/provider/prabsyn.cmi

