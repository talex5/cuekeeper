FROM ocaml/opam2:debian-10-opam@sha256:79636246d69ee9d285c87af8a078d2dcc1c893d75e40c581da3284da6cf1841c
#FROM ocaml/opam2:debian-10-opam
ENV DEBIAN_FRONTEND=noninteractive
RUN sudo apt-get -y update && sudo apt-get -y install aspcud zip m4 autoconf build-essential gcc-multilib ca-certificates git rsync time --no-install-recommends
RUN opam init --comp=4.04.2+32bit --disable-sandboxing
RUN opam pin add -n reactiveData https://github.com/hhugo/reactiveData.git
RUN mkdir /home/opam/cuekeeper
COPY --chown=opam opam /home/opam/cuekeeper/opam
WORKDIR /home/opam/cuekeeper
RUN opam install --solver=aspcud -y -t --deps-only .
ENTRYPOINT ["opam", "config", "exec", "--"]
