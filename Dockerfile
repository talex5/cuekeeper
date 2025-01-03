FROM ocaml/opam:debian-12-ocaml-4.14@sha256:bc657956e4dfb5cd9b822594dfd894b293c7068aa69b6f0a5ad31638a02890bf
#FROM ocurrent/opam:debian-12-ocaml-4.14
# Make sure we're using opam-2.3:
RUN sudo ln -sf /usr/bin/opam-2.3 /usr/bin/opam && opam option solver=builtin-0install
RUN mkdir /home/opam/cuekeeper
COPY --chown=opam cuekeeper.opam /home/opam/cuekeeper/
WORKDIR /home/opam/cuekeeper
RUN opam pin add -yn cuekeeper.dev .
RUN opam install -t --deps-only cuekeeper
ENTRYPOINT ["opam", "exec", "--"]
