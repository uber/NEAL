PWD = $(pwd)


include Makefile.propagate

register:
	opam exec -- ocamlfind remove neal
	opam exec -- ocamlfind install neal \
		$(PWD)/opam/META \
		$(PWD)/core/_build/neal.cmi \
		$(PWD)/core/_build/driver/fs.cmo \
		$(PWD)/core/_build/driver/fs.cmi \
		$(PWD)/core/_build/provider/prabsyn.cmo \
		$(PWD)/core/_build/provider/prabsyn.cmi

