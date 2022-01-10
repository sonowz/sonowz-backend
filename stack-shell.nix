{ ghc }:
with import <nixpkgs> {};

# Nix build environment is included for making images
haskell.lib.buildStackProject {
  inherit ghc;
  name = "sonowz-backend";
  buildInputs = [
    postgresql
    zlib
  ];
}

