let sources = import nix/sources.nix; in
# This module is intended to be called with ‘nixpkgs.callPackage’
{ callPackage
, haskellPackages
, haskell
, coreutils
, utillinux

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}
}:
let
  inherit (__nix-utils) esc writeCheckedExecutable shellCheckers;
  inherit (haskell.lib) justStaticExecutables;
  hp = haskellPackages;

  # Shortcuts for executables that are already escaped for Bash
  c = builtins.mapAttrs (n: v: esc v) executables;

  assetsCommit = "b636cb9651c6ededbbda80489c79ff4a68653d96";

  staticAssetUrl = filePath:
    "https://raw.githubusercontent.com/unclechu/place-cursor-at/" +
    "${assetsCommit}/${filePath}";

  gitHubUrl = filePath:
    "https://github.com/unclechu/place-cursor-at/blob/" +
    "${assetsCommit}/${filePath}";

  executables =
    builtins.foldl'
      (acc: x: acc // { ${x} = "${coreutils}/bin/${x}"; })
      {
        hpack = "${justStaticExecutables hp.hpack}/bin/hpack";
        cabal = "${justStaticExecutables hp.cabal-install}/bin/cabal";
        column = "${utillinux}/bin/column";
      }
      [ "cp" "mv" "mktemp" "basename" ];
in
#
# This script makes some preparations for publishing on Hackage and then calls
# ‘cabal sdist’.
#
# Those preparations include:
#
#   1. Replace relative links to some files in ‘README.md’ with links to static
#      content on GitHub (or to normal GitHub pages for some cases). Those files
#      aren’t packaged into ‘sdist’ bundle so the links would be just broken
#      otherwise. New links will be attached to a specific commit so the content
#      stays the same.
#
#   2. Replace Markdown table in ‘README.md’ with just ASCII table wrapped into
#      a code block
#
writeCheckedExecutable "custom-sdist" ''
  ${
    builtins.concatStringsSep "\n"
      (builtins.map shellCheckers.fileIsExecutable
        (builtins.attrValues executables))
  }
'' ''
  set -Eeuxo pipefail || exit

  # Generate fresh ‘*.cabal’ file from ‘package.yaml’
  ${c.hpack}

  TMP_FILE_PATH=$(${c.mktemp} -ut 'README.md.bakXXXXXX')
  README_BAK_FILE_NAME=$(${c.basename} -- "$TMP_FILE_PATH")

  # Make a backup of ‘README.md’ before patching it
  ${c.cp} README.md -- "$README_BAK_FILE_NAME"

  readarray -t LINES < README.md

  # Restore original ‘README.md’ from backup file
  restore-readme-backup() {
    set -Eeuxo pipefail || exit
    ${c.mv} -f -- "$README_BAK_FILE_NAME" README.md
  }

  # Encapsulate ‘trap’
  (
    trap restore-readme-backup EXIT
    > README.md # Empty the file first

    table=

    # Encapsulate stdout redirect and ‘set’
    (
      exec >> README.md # Redirect stdout to the README file
      set +x # Too much noise

      for line in "''${LINES[@]}"; do

        # End of table, fix the table for Hackage and print it
        if [[ -n $table ]] && ! [[ $line =~ ^\| ]]; then
          echo '```'
          table=''${table//'`'/}
          table=''${table//'*'/}
          table=$(${c.column} -t -o ' ' <<< "$table")
          printf '%s\n' "$table"
          echo '```'
          table=
          continue

        # Save table into a variable
        elif [[ $line =~ ^\| ]]; then
          table="$table$line"$'\n'
          continue
        fi

        line=''${line//'(artwork/screenshot.png)'/(${
          esc (staticAssetUrl "artwork/screenshot.png")
        })}

        line=''${line//'(artwork/logo/horizontal.svg)'/(${
          esc (staticAssetUrl "artwork/logo/horizontal.svg")
        })}
        line=''${line//'(artwork/logo/horizontal.png)'/(${
          esc (staticAssetUrl "artwork/logo/horizontal.png")
        })}
        line=''${line//'(artwork/logo/vertical.svg)'/(${
          esc (staticAssetUrl "artwork/logo/vertical.svg")
        })}
        line=''${line//'(artwork/logo/vertical.png)'/(${
          esc (staticAssetUrl "artwork/logo/vertical.png")
        })}
        line=''${line//'(artwork/logo/icon.svg)'/(${
          esc (staticAssetUrl "artwork/logo/icon.svg")
        })}
        line=''${line//'(artwork/logo/icon.png)'/(${
          esc (staticAssetUrl "artwork/logo/icon.png")
        })}

        line=''${line//'(LICENSE)'/(${esc (staticAssetUrl "LICENSE")})}

        line=''${line//': shell.nix'/: ${esc (gitHubUrl "shell.nix")}}
        line=''${line//': artwork/logo'/: ${esc (gitHubUrl "artwork/logo")}}

        printf '%s\n' "$line"
      done
    )

    ${c.cabal} sdist
  )
''
