let sources = import nix/sources.nix; in
{ pkgs ? import sources.nixpkgs {}
, src ? (import nix/clean-src.nix { inherit pkgs; }) ./. # A directory
, justStaticExecutable ? true
}:
let
  name = "place-cursor-at";
  pkg = haskellPackages.callCabal2nix name src {};

  haskellPackages = pkgs.haskellPackages.extend (self: super: {
    ${name} = pkg;
  });

  justStaticExecutableFn =
    if justStaticExecutable
    then pkgs.haskell.lib.justStaticExecutables
    else x: x;
in
justStaticExecutableFn pkg // {
  inherit src haskellPackages;
  haskellPackage = pkg;
}
