CAML_LD_LIBRARY_PATH = $(shell opam config var prefix)/lib/stublibs/
export CAML_LD_LIBRARY_PATH

# make JFLAGS="--pretty --noinline"
JFLAGS =

VERSION = dev
RELEASE_DIR = cuekeeper-bin-${VERSION}
MIRAGE_FLAGS = --unix

client-test: client test

.PHONY: build-byte test server
client: _build/js/client.js

test:
	ocamlbuild -cflag -g -no-links -use-ocamlfind client.byte tests/test.byte
	./_build/tests/test.byte

build-byte: ck_init.ml
	ocamlbuild -cflag -g -no-links -use-ocamlfind client.byte

_build/js/client.js: build-byte
	js_of_ocaml ${JFLAGS} +weak.js +cstruct/cstruct.js +bin_prot.js _build/js/client.byte

slow_test:
	ocamlbuild -cflag -g -no-links -use-ocamlfind tests/test.native
	env CK_TEST_ITERS=10000 ./_build/tests/test.native

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

docker-build:
	docker build -t cuekeeper .
	docker run --rm -v $(CURDIR):/home/opam/cuekeeper cuekeeper make

server/conf/server.key:
	@echo Generating server key...
	[ -d server/conf] || mkdir -p server/conf
	openssl genpkey -out $@ -outform PEM -algorithm RSA -pkeyopt rsa_keygen_bits:4096

server/conf/server.pem: server/conf/server.key
	@echo ">>> Generating server X.509 certificate."
	@echo ">>> Enter the server's full hostname as the 'Common Name' (e.g. cuekeeper.mynet)."
	@echo ">>> Everything else can be left blank."
	@echo
	@openssl req -new -x509 -key $< -out $@ -days 10000

server: client server/conf/server.pem
	rm -rf _build/static
	mkdir _build/static
	cp -r resources _build/static/
	cp _build/js/client.js _build/static/resources/js/cuekeeper.js
	sed 's!_build/js/client.js!resources/js/cuekeeper.js!;s!var ck_use_server=false;!var ck_use_server=true;!' test.html > _build/static/index.html
	ocaml-crunch _build/static -e html -e js -e css -e ico -o server/static.ml -m plain
	(cd server && mirage configure ${MIRAGE_FLAGS} && make)

clean:
	ocamlbuild -clean
