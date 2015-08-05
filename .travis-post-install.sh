#!/bin/bash -eux
make server/conf/tls/server.key
touch server/conf/tls/server.pem
opam install mirage
ocamlfind list
opam list
make clean
cp server/devices.ml.example server/devices.ml
make server MIRAGE_FLAGS='--no-depext'
