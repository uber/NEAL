include src/Makefile.propagate

setup-buildonly:
	opam update
	opam pin add neal src -n -y
	opam install --deps-only neal -y

install-brew:
	# install the main binary
	mkdir -p "$(BIN_PATH)"
	cp src/core/_build/main.native "$(BIN_PATH)/neal"
	# setup directories for components
	mkdir -p $(LIB_PATH)/"reporters"
	mkdir -p $(LIB_PATH)/"providers/helpers"
	# install providers and helpers
	cp $(wildcard src/providers/*/_build/*.provider.cmxs) "$(LIB_PATH)/providers"
	cp $(wildcard src/providers/helpers/*) "$(LIB_PATH)/providers/helpers"
	# install reporters
	for reporter in $(shell find src/reporters -type d -not -name '_build' -mindepth 1); do \
		dest="$(LIB_PATH)/reporters/$$(basename $$reporter)"; \
		mkdir -p "$$dest"; \
		cp $$(find $$reporter/_build -name '*.reporter.cmxs') $$dest; \
	done

brew: setup-buildonly
	make -C src/core
	make -C src/providers
	make -C src/reporters
	make install-brew

setup: setup-buildonly
	virtualenv env
	. $$PWD/env/bin/activate; \
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
	@opam exec -- bisect-ppx-report html $$(find . -name '*.coverage') $$(find . -name _build -type d -exec echo --source-path {} \;)
	@find . -name '*.coverage' | xargs rm -r
