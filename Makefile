CAML_LD_LIBRARY_PATH = $(shell opam config var prefix)/lib/stublibs/
export CAML_LD_LIBRARY_PATH

.PHONY: build test
all: test _build/js/client.js

test: build-byte
	ocamlrun _build/test.byte

build-byte:
	ocamlbuild -cflag -g -no-links -use-ocamlfind client.byte test.byte

_build/js/client.js: build-byte
	js_of_ocaml +weak.js js/helpers.js _build/js/client.byte

clean:
	ocamlbuild -clean
