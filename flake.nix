{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  inputs.opam-nix.url = "github:tweag/opam-nix";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = {self, nixpkgs, flake-utils, opam-nix, ...}: flake-utils.lib.eachDefaultSystem (system: let
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    packages.default = let
      opam-nix-lib = opam-nix.lib.${system};
      inherit (opam-nix-lib) buildOpamProject' materializedDefsToScope;
      packages = (buildOpamProject' {} ./. {
        ocaml-base-compiler = "*";
      }).overrideScope (final: prev: {
        cuekeeper = prev.cuekeeper.overrideAttrs {
          postInstall = ''
            cp -r _build $out/build
            mkdir -p $out/bin
            cp ${./cuekeeper} $out/bin/cuekeeper
          '';
        };
      });
    in
      packages.cuekeeper
    ;
    devShells.default = pkgs.mkShell {
      packages = with pkgs; [
        gnumake
        opam
        dune_3
        pkg-config
        openssl
        gmp
      ];
    };
  });
}
