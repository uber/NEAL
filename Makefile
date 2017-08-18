include src/Makefile.propagate

setup:
	opam update
	opam pin add neal src -n -y
	opam install --deps-only neal -y
	virtualenv env
	source $$PWD/env/bin/activate; \
	pip install -r tests/requirements.txt

trim:
	@for file in $$(git ls-files); do \ if [[  ! -L "./$$file" ]]; then \
			sed -i '' -E "s/[[:space:]]*$$//" $$file; \
		fi \
	done

register:
	$(MAKE) -C src register

install:
	@ln -fs $(NEAL) /usr/local/bin/neal
	@echo installed at /usr/local/bin/neal

coverage:
	@COVERAGE=1 $(MAKE) test
	@bisect-ppx-report -html src/core/_build/coverage/ $$(find . -name _build -type d -exec echo -I {} \;) $$(find . -name 'bisect*.out')
	@find . -name 'bisect*out' | xargs rm
