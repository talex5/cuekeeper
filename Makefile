CAML_LD_LIBRARY_PATH = $(shell opam config var prefix)/lib/stublibs/
export CAML_LD_LIBRARY_PATH

.PHONY: all
all:
	ocamlbuild -no-links -use-ocamlfind client.byte
	js_of_ocaml +weak.js helpers.js _build/client.byte

clean:
	ocamlbuild -clean
