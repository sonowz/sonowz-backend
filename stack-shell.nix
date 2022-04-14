{ ghc }:
with import <nixpkgs> {};

haskell.lib.buildStackProject {
  inherit ghc;
  name = "sonowz-backend";
  buildInputs = pkgs.callPackage ./requirements.nix {};
}

