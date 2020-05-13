FROM ocurrent/opam:debian-10-ocaml-4.07@sha256:2f9a9fc195b29f1006d0f97ae4c6300fb175800703b12a15b51583a1f7a547b7
#FROM ocurrent/opam:debian-10-ocaml-4.07
RUN mkdir /home/opam/cuekeeper
COPY --chown=opam cuekeeper.opam /home/opam/cuekeeper/
WORKDIR /home/opam/cuekeeper
RUN opam pin add -yn cuekeeper.dev .
RUN opam depext -t cuekeeper
RUN opam install -t --deps-only cuekeeper
ENTRYPOINT ["opam", "config", "exec", "--"]
