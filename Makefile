CAML_LD_LIBRARY_PATH = $(shell opam config var prefix)/lib/stublibs/
export CAML_LD_LIBRARY_PATH

# make JFLAGS="--pretty --noinline"
JFLAGS =

.PHONY: build test
all: test _build/js/client.js

test: build-byte
	ocamlrun _build/test.byte

build-byte: ck_init.ml
	ocamlbuild -cflag -g -no-links -use-ocamlfind client.byte test.byte

_build/js/client.js: build-byte
	js_of_ocaml ${JFLAGS} +weak.js js/helpers.js _build/js/client.byte

slow_test:
	ocamlbuild -cflag -g -no-links -use-ocamlfind test.native
	env CK_TEST_ITERS=10000 ./_build/test.native

ck_init.ml: init/*/*
	ocaml-crunch init -o ck_init.ml -m plain

clean:
	ocamlbuild -clean
