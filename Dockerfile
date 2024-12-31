FROM ocaml/opam:debian-12-ocaml-4.10@sha256:0bfba59a888af2a366cdb92a1fa7906446f062d8d38a95050f730a3aa0c5acea
#FROM ocurrent/opam:debian-12-ocaml-4.10
# Make sure we're using opam-2.3:
RUN sudo ln -sf /usr/bin/opam-2.3 /usr/bin/opam && opam option solver=builtin-0install
RUN mkdir /home/opam/cuekeeper
COPY --chown=opam cuekeeper.opam /home/opam/cuekeeper/
WORKDIR /home/opam/cuekeeper
RUN opam pin add -yn cuekeeper.dev .
RUN opam install -t --deps-only cuekeeper
ENTRYPOINT ["opam", "exec", "--"]
