# nix-shell --command "ghci -ferror-spans"

{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  ghc = nixpkgs.haskellPackages.ghcWithPackages (ps: with ps; [
          groups
        ]);
in
pkgs.stdenv.mkDerivation {
  name = "dddfp";
  buildInputs = [ ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
