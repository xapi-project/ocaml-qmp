build: setup.data
	ocaml setup.ml -build $(BUILDFLAGS)

doc: setup.data build
	ocaml setup.ml -doc $(DOCFLAGS)

test: setup.data build
	ocaml setup.ml -test $(TESTFLAGS)

all: setup.ml
	ocaml setup.ml -all $(ALLFLAGS)

install: setup.data
	ocaml setup.ml -install $(INSTALLFLAGS)

uninstall: setup.data
	ocaml setup.ml -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	ocaml setup.ml -reinstall $(REINSTALLFLAGS)

clean: setup.ml
	ocaml setup.ml -clean $(CLEANFLAGS)

distclean: setup.ml
	ocaml setup.ml -distclean $(DISTCLEANFLAGS)

setup.data: setup.ml
	ocaml setup.ml -configure $(CONFIGUREFLAGS)

configure: setup.ml
	ocaml setup.ml -configure $(CONFIGUREFLAGS)

setup.ml: _oasis
	oasis setup

.PHONY: build doc test all install uninstall reinstall clean distclean configure
