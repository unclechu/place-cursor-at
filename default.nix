{ callPackage
, haskellPackages
, haskell

# Build options
, __src ? (callPackage nix/clean-src.nix {}) ./. # A directory
, justStaticExecutable ? true
}:
let
  name = "place-cursor-at";
  pkg = extendedHaskellPackages.callCabal2nix name __src {};

  extendedHaskellPackages = haskellPackages.extend (self: super: {
    ${name} = pkg;
  });

  justStaticExecutableFn =
    if justStaticExecutable
    then haskell.lib.justStaticExecutables
    else x: x;
in
justStaticExecutableFn pkg // {
  haskellPackages = extendedHaskellPackages;
  haskellPackage = pkg;
}
