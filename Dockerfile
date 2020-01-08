FROM ocurrent/opam:debian-10-ocaml-4.07@sha256:2f9a9fc195b29f1006d0f97ae4c6300fb175800703b12a15b51583a1f7a547b7
#FROM ocurrent/opam:debian-10-ocaml-4.07
RUN opam pin add -yn reactiveData https://github.com/hhugo/reactiveData.git
RUN opam pin add -yn irmin-git.1.4.0 https://github.com/talex5/irmin.git#1.4.0-cuekeeper
RUN opam pin add -yn irmin-indexeddb.1.3 https://github.com/talex5/irmin-indexeddb.git#irmin-1.3
RUN mkdir /home/opam/cuekeeper
COPY --chown=opam cuekeeper.opam /home/opam/cuekeeper/
WORKDIR /home/opam/cuekeeper
RUN opam pin add -yn cuekeeper.dev .
RUN opam depext -t cuekeeper
RUN opam install -t --deps-only cuekeeper
ENTRYPOINT ["opam", "config", "exec", "--"]
