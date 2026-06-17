# Author: Viacheslav Lotsmanov
# License: GNU/GPLv3 https://raw.githubusercontent.com/unclechu/place-cursor-at/master/LICENSE

# In order to generate ‘cabal sdist’ bundle for Hackage just run:
#   nix-shell --arg withCustomSdistScript true --run custom-sdist

let
  sources = import nix/sources.nix;
  currentNixpkgsRelease = "26.05";
in

args@
{ pkgs ?
    import sources."nixpkgs${
      if nixpkgs-release == currentNixpkgsRelease then "" else "-${nixpkgs-release}"
    }" {}

# Tested with lower 19.03 and upper 26.05 pins.
# One of: '26.05', '23.05', '20.09', '20.03', '19.09', '19.03'
# This field only matters if you don’t override `pkgs` argument.
, nixpkgs-release ? currentNixpkgsRelease

# It’s not provided in 19.03 for instance
, yq-go ? (import sources.nixpkgs { inherit (pkgs) config; }).yq-go

# Forwarded build options
, __src ? null
, justStaticExecutable ? false # N.B. Default value is different here

# Overridable local dependencies
, nix-utils ? pkgs.callPackage sources.nix-utils {}
, custom-sdist-script ?
    pkgs.callPackage nix/custom-sdist-script.nix { __nix-utils = nix-utils; }

# Local arguments
, withHaskellLanguageServer ? true
, withCabal ? true
, withHpack ? false
, withStack ? false
, withStackNixDependencies ? false
, withPackageRepl ? false # Adds package library modules into GHCi REPL
, withHoogle ? true
, withCustomSdistScript ? false
, buildExecutable ? true
}:

assert withHaskellLanguageServer -> withCabal;

let
  inherit (pkgs) lib;
  forwardedNames = [ "__src" ];
  filterForwarded = lib.filterAttrs (n: v: builtins.elem n forwardedNames);
  forwardedArgs = { inherit justStaticExecutable; } // filterForwarded args;
  pkg = pkgs.callPackage ./. forwardedArgs;
  hp = pkg.haskellPackages;
  name = pkg.haskellPackage.pname;

  inherit (nix-utils) esc wrapExecutable;
  pkgReplGhc = hp.ghcWithPackages (p: [p.${name}]);

  # LSP for Haskell
  hls = (pkgs.haskell-language-server.override {
    haskellPackages = hp;

    supportedGhcVersions = [
      (builtins.replaceStrings ["."] [""] pkgReplGhc.version)
    ];
  });

  # Produces ‘PACKAGE-NAME-ghc’ and ‘PACKAGE-NAME-ghci’ files.
  # ‘shellFor’ overrides ‘ghc’ and ‘ghci’ executables.
  pkgRepl =
    let
      exe = binName:
        wrapExecutable
          "${pkgReplGhc}/bin/${binName}"
          { name = "${name}-${binName}"; };
    in [
      (exe "ghci")
      (exe "ghc")
    ];

  stackNixDependencies =
    let
      stackYamlFile = "${./stack.yaml}";
      path = "nix.packages";

      nixModule = pkgs.runCommand "${name}-stack-yaml-nix-packages" {} ''
        set -Eeuo pipefail || exit
        JSON=$(${esc yq-go}/bin/yq r -j -- ${esc stackYamlFile} ${esc path})

        if [[ -z $JSON ]]; then
          >&2 printf 'Failed to extract "%s" from "%s"!\n' \
            ${esc path} ${esc stackYamlFile}
          exit 1
        fi

        printf '%s' "$JSON" > "$out"
      '';

      attrPathToDerivation = x:
        lib.attrsets.getAttrFromPath (lib.splitString "." x) pkgs;

      attrPaths = builtins.fromJSON (builtins.readFile nixModule);
    in
      assert builtins.isList attrPaths;
      assert builtins.length attrPaths > 0;
      assert builtins.all builtins.isString attrPaths;
      map attrPathToDerivation attrPaths;

  hpack = pkgs.haskell.lib.justStaticExecutables hp.hpack;
  cabal = pkgs.haskell.lib.justStaticExecutables hp.cabal-install;
  stack = pkgs.haskell.lib.justStaticExecutables hp.stack;
in

hp.shellFor {
  packages = p: [
    p.${name}
  ];

  inherit withHoogle;

  buildInputs =
    lib.optional withHaskellLanguageServer hls ++
    lib.optional withCabal cabal ++
    lib.optional withHpack hpack ++
    lib.optional withStack stack ++
    lib.optionals withStackNixDependencies stackNixDependencies ++
    lib.optionals withPackageRepl pkgRepl ++
    lib.optional withCustomSdistScript custom-sdist-script ++
    lib.optional buildExecutable hp.${name};
}
