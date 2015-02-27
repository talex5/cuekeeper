CAML_LD_LIBRARY_PATH = $(shell opam config var prefix)/lib/stublibs/
export CAML_LD_LIBRARY_PATH

.PHONY: all
all:
	ocamlbuild -cflag -g -no-links -use-ocamlfind client.byte test.byte
	ocamlrun _build/test.byte
	js_of_ocaml +weak.js helpers.js _build/client.byte

clean:
	ocamlbuild -clean
