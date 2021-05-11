PWD = $(pwd)

include Makefile.propagate


register:
	opam exec -- pushd $(PWD) && ocamlfind remove neal
	opam exec -- pushd $(PWD) && ocamlfind install neal \
		opam/META \
		core/_build/neal.cmi \
		core/_build/driver/fs.cmo \
		core/_build/driver/fs.cmi \
		core/_build/provider/prabsyn.cmo \
		core/_build/provider/prabsyn.cmi

