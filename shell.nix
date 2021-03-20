# Author: Viacheslav Lotsmanov
# License: GNU/GPLv3 https://raw.githubusercontent.com/unclechu/place-cursor-at/master/LICENSE

let sources = import nix/sources.nix; in
args@
{ pkgs ?
    import sources."nixpkgs${
      if nixpkgs-release == "20.09" then "" else "-${nixpkgs-release}"
    }" {}

# Tested with lower 19.03 and upper 20.09 pins.
, nixpkgs-release ? "20.09" # One of: '20.09', '20.03', '19.09', '19.03'

# It’s not provided in 19.03 for instance
, yq-go ? (import sources.nixpkgs { inherit (pkgs) config; }).yq-go

# Forwarded build options
, __src ? null
, justStaticExecutable ? false # N.B. Default value is different here

# Overridable local dependencies
, nix-utils ? pkgs.callPackage sources.nix-utils {}

# Local arguments
, withCabal ? false
, withStack ? false
, withStackNixDependencies ? false
, withPackageRepl ? false # Adds package library modules into GHCi REPL
, withHoogle ? true
, buildExecutable ? true
}:
let
  forwardedNames = [ "__src" ];
  filterForwarded = pkgs.lib.filterAttrs (n: v: builtins.elem n forwardedNames);
  forwardedArgs = { inherit justStaticExecutable; } // filterForwarded args;
  pkg = pkgs.callPackage ./. forwardedArgs;
  hp = pkg.haskellPackages;
  name = pkg.haskellPackage.pname;

  inherit (nix-utils) esc wrapExecutable;
  pkgReplGhc = hp.ghcWithPackages (p: [p.${name}]);

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
        pkgs.lib.attrsets.getAttrFromPath (pkgs.lib.splitString "." x) pkgs;

      attrPaths = builtins.fromJSON (builtins.readFile nixModule);
    in
      assert builtins.isList attrPaths;
      assert builtins.length attrPaths > 0;
      assert builtins.all builtins.isString attrPaths;
      map attrPathToDerivation attrPaths;
in
hp.shellFor {
  packages = p: [
    p.${name}
  ];

  inherit withHoogle;

  buildInputs =
    (if withCabal then [ hp.cabal-install ] else []) ++
    (if withStack then [ hp.stack ] else []) ++
    (if withStackNixDependencies then stackNixDependencies else []) ++
    (if buildExecutable then [ hp.${name} ] else []) ++
    (if withPackageRepl then pkgRepl else []);
}
