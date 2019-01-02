# vim: set noet ts=8:
#
# This Makefile is not called from Opam but only used for
# convenience during development
#

PROFILE=release

.PHONY: build install uninstall clean test doc reindent

build:
	dune build @install --profile=$(PROFILE)

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean

test:
	dune runtest --profile=$(PROFILE)

doc:
	dune build @doc --profile=$(PROFILE)

reindent:
	ocp-indent --inplace **/*.ml*
