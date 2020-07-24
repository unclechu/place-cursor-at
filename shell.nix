{ pkgs ? import <nixpkgs> {} }:
let place-cursor-at = import ./. { inherit pkgs; };
in  pkgs.mkShell { buildInputs = [ place-cursor-at ]; }
