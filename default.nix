{ pkgs ? import <nixpkgs> {} }:
let pkg = pkgs.haskellPackages.callCabal2nix "place-cursor-at" ./. {};
in  pkgs.haskell.lib.justStaticExecutables pkg // { haskell-pkg = pkg; }
