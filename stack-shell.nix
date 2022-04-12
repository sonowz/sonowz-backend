{ ghc }:
with import <nixpkgs> {};

haskell.lib.buildStackProject {
  inherit ghc;
  name = "sonowz-backend";
  buildInputs = [
    flac
    postgresql
    taglib
    zlib
  ];
}

