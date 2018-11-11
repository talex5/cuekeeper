FROM debian:stretch
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get -y update && apt-get -y upgrade && apt-get -y install aspcud zip m4 autoconf opam build-essential gcc-multilib ca-certificates git rsync time --no-install-recommends
RUN opam init --comp=4.04.2+32bit
RUN opam pin add -n reactiveData https://github.com/hhugo/reactiveData.git
ADD opam /home/opam/cuekeeper/opam
WORKDIR /home/opam/cuekeeper
RUN opam pin add -n -y cuekeeper .
RUN opam install -y mirage-types-lwt mirage-http ocamlbuild 'conduit=0.13.0' 'lwt<2.7.0' camlp4 react
RUN opam install -y --deps-only cuekeeper
ENTRYPOINT ["opam", "config", "exec", "--"]
