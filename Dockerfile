FROM ocaml/opam:debian-12-ocaml-4.07@sha256:a887442d9759c6062440ede489808ac5a381efa0b7048582bc45d813de826ea7
#FROM ocurrent/opam:debian-12-ocaml-4.07
# Make sure we're using opam-2.2:
RUN sudo ln -sf /usr/bin/opam-2.2 /usr/bin/opam
RUN mkdir /home/opam/cuekeeper
COPY --chown=opam cuekeeper.opam /home/opam/cuekeeper/
WORKDIR /home/opam/cuekeeper
RUN opam pin add -yn cuekeeper.dev .
RUN opam install -t --deps-only cuekeeper
ENTRYPOINT ["opam", "exec", "--"]
