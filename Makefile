CAML_LD_LIBRARY_PATH = $(shell opam config var prefix)/lib/stublibs/
export CAML_LD_LIBRARY_PATH

# make JFLAGS="--pretty --noinline"
JFLAGS =

VERSION = dev
RELEASE_DIR = cuekeeper-bin-${VERSION}
MIRAGE_FLAGS = --unix

all: client

.PHONY: build test server
client: test _build/js/client.js

test: build-byte
	./_build/test.byte

build-byte: ck_init.ml
	ocamlbuild -cflag -g -no-links -use-ocamlfind client.byte test.byte

_build/js/client.js: build-byte
	js_of_ocaml ${JFLAGS} +weak.js js/helpers.js _build/js/client.byte

slow_test:
	ocamlbuild -cflag -g -no-links -use-ocamlfind test.native
	env CK_TEST_ITERS=10000 ./_build/test.native

ck_init.ml: init/*/*
	ocaml-crunch init -o ck_init.ml -m plain

release:
	rm -rf "${RELEASE_DIR}"
	mkdir "${RELEASE_DIR}"
	git archive HEAD --format=tar resources LICENSE | tar  xf - -C "${RELEASE_DIR}"
	cp _build/js/client.js "${RELEASE_DIR}/resources/js/cuekeeper.js"
	sed 's!_build/js/client.js!resources/js/cuekeeper.js!' test.html > "${RELEASE_DIR}/index.html"
	sed '/^Installation/,/^Instructions/{/^Instructions/!d}' README.md > "${RELEASE_DIR}/README.md"
	zip -r "${RELEASE_DIR}.zip" ${RELEASE_DIR}
	rm -rf "${RELEASE_DIR}"

server: client
	rm -rf _build/static
	mkdir _build/static
	cp -r resources _build/static/
	cp _build/js/client.js _build/static/resources/js/cuekeeper.js
	sed 's!_build/js/client.js!resources/js/cuekeeper.js!;s!var ck_use_server=false;!var ck_use_server=true;!' test.html > _build/static/index.html
	ocaml-crunch _build/static -e html -e js -e css -e ico -o server/static.ml -m plain
	(cd server && mirage configure ${MIRAGE_FLAGS} && make)

clean:
	ocamlbuild -clean
